import streamlit as st
import pandas as pd
import ast
import networkx as nx
import plotly.graph_objects as go
import plotly.express as px

st.set_page_config(page_title="Skincare Ingredient Explorer", layout="wide")

# ---------- Data loading ----------
@st.cache_data
def load_data():
    ing = pd.read_csv("data/ingredients_master_enriched.csv")
    rules = pd.read_csv("data/compatibility_rules.csv")
    products = pd.read_csv("data/products_clean.csv")
    products["ingredient_list"] = products["ingredient_list"].apply(ast.literal_eval)
    return ing, rules, products

ing, rules, products = load_data()

STATUS_COLORS = {"synergy": "#2E8B57", "caution": "#E8A33D", "avoid": "#C0392B"}


def find_products(alias, top_n=6):
    """Real product lookup: substring-matches an ingredient alias against
    each product's actual ingredient list. Returns an empty result honestly
    if the ingredient genuinely isn't in this dataset."""
    alias = alias.lower()
    mask = products["ingredient_list"].apply(
        lambda lst: any(alias in i.strip().lower() for i in lst)
    )
    return products[mask].head(top_n)


st.title("🧴 Interactive Skincare Ingredient & Compatibility Explorer")
st.caption(
    "Real product & ingredient data from a public Kaggle dataset (1,138 products), "
    "enriched with real ingredient function data where available. Skin-type fit and "
    "compatibility rules are a clearly-labeled researcher-curated layer — see About tab."
)

tabs = st.tabs([
    "🕸️ Network Graph",
    "🔥 Compatibility Heatmap",
    "🎯 Skin-Type Fit (Radar)",
    "📊 Real Usage Frequency",
    "⚗️ pH vs Irritation (Scatter)",
    "🛍️ Compatibility Checker + Recommendations",
])

st.sidebar.header("Filters")
concern_filter = st.sidebar.multiselect("Filter by concern", sorted(ing["concern"].unique()), default=[])
category_filter = st.sidebar.multiselect("Filter by category", sorted(ing["category"].unique()), default=[])

filtered_ing = ing.copy()
if concern_filter:
    filtered_ing = filtered_ing[filtered_ing["concern"].isin(concern_filter)]
if category_filter:
    filtered_ing = filtered_ing[filtered_ing["category"].isin(category_filter)]

filtered_names = set(filtered_ing["ingredient"])
filtered_rules = rules[rules["ingredient_a"].isin(filtered_names) & rules["ingredient_b"].isin(filtered_names)]

# ================= TAB 1: Network Graph =================
with tabs[0]:
    st.subheader("Ingredient Compatibility Network")
    st.write("🟢 synergy · 🟡 caution · 🔴 avoid — hover for the reason. (Curated relationship data.)")

    G = nx.Graph()
    for _, row in filtered_ing.iterrows():
        G.add_node(row["ingredient"], category=row["category"])
    for _, row in filtered_rules.iterrows():
        G.add_edge(row["ingredient_a"], row["ingredient_b"], status=row["status"], reason=row["reason"])

    if G.number_of_nodes() == 0:
        st.info("No ingredients match the current filters.")
    else:
        pos = nx.spring_layout(G, seed=42, k=0.8)
        edge_traces = []
        for status, color in STATUS_COLORS.items():
            ex, ey = [], []
            for u, v, data in G.edges(data=True):
                if data["status"] != status:
                    continue
                x0, y0 = pos[u]; x1, y1 = pos[v]
                ex += [x0, x1, None]; ey += [y0, y1, None]
            edge_traces.append(go.Scatter(x=ex, y=ey, mode="lines",
                                           line=dict(width=2, color=color), name=status, hoverinfo="skip"))

        node_x = [pos[n][0] for n in G.nodes()]
        node_y = [pos[n][1] for n in G.nodes()]
        degrees = [G.degree(n) for n in G.nodes()]
        node_trace = go.Scatter(
            x=node_x, y=node_y, mode="markers+text", text=[n for n in G.nodes()],
            textposition="top center", textfont=dict(size=9),
            hovertext=[f"{n} — {G.degree(n)} connections" for n in G.nodes()], hoverinfo="text",
            marker=dict(size=[12 + d * 3 for d in degrees], color="#4A6FA5", line=dict(width=1, color="white")),
            name="ingredient"
        )
        fig = go.Figure(data=edge_traces + [node_trace])
        fig.update_layout(showlegend=True, height=650,
                           xaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
                           yaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
                           margin=dict(l=10, r=10, t=10, b=10))
        st.plotly_chart(fig, use_container_width=True)

# ================= TAB 2: Heatmap =================
with tabs[1]:
    st.subheader("Ingredient × Ingredient Compatibility Matrix")
    names = sorted(filtered_names)
    status_val = {"avoid": -1, "caution": 0, "synergy": 1}
    matrix = pd.DataFrame(0, index=names, columns=names, dtype=float)
    for _, row in filtered_rules.iterrows():
        a, b, s = row["ingredient_a"], row["ingredient_b"], row["status"]
        if a in names and b in names:
            matrix.loc[a, b] = status_val[s]; matrix.loc[b, a] = status_val[s]
    if names:
        fig = px.imshow(matrix, color_continuous_scale=["#C0392B", "#E8A33D", "#2E8B57"], zmin=-1, zmax=1, aspect="auto")
        fig.update_layout(height=650, coloraxis_colorbar=dict(title="Status", tickvals=[-1, 0, 1], ticktext=["Avoid", "Caution", "Synergy"]))
        st.plotly_chart(fig, use_container_width=True)
        st.caption("Cells with no known rule show as neutral — that means 'no data', not 'confirmed safe'.")
    else:
        st.info("No ingredients match the current filters.")

# ================= TAB 3: Radar =================
with tabs[2]:
    st.subheader("Skin-Type Fit by Ingredient")
    st.caption("Curated scores (1-5), not from the product dataset.")
    options = sorted(filtered_ing["ingredient"].unique())
    selected = st.multiselect("Select ingredients to compare", options, default=options[:4] if len(options) >= 4 else options)
    if selected:
        categories = ["oily", "dry", "combination", "sensitive"]
        fig = go.Figure()
        for name in selected:
            row = ing[ing["ingredient"] == name].iloc[0]
            values = [row[c] for c in categories] + [row[categories[0]]]
            fig.add_trace(go.Scatterpolar(r=values, theta=categories + [categories[0]], fill="toself", name=name))
        fig.update_layout(polar=dict(radialaxis=dict(visible=True, range=[0, 5])), height=600, showlegend=True)
        st.plotly_chart(fig, use_container_width=True)
    else:
        st.info("Select at least one ingredient above.")

# ================= TAB 4: Real usage frequency =================
with tabs[3]:
    st.subheader("How Often Each Ingredient Actually Appears (Real Data)")
    st.caption(f"Counted directly from the {len(products)}-product dataset — not estimated.")
    view = st.radio("View as", ["Bar chart", "Treemap"], horizontal=True)
    plot_df = filtered_ing.sort_values("real_usage_count", ascending=True)
    if plot_df.empty:
        st.info("No ingredients match the current filters.")
    elif view == "Bar chart":
        fig = px.bar(plot_df, x="real_usage_count", y="ingredient", color="category",
                     orientation="h", height=max(400, len(plot_df) * 22))
        fig.update_layout(yaxis_title="", xaxis_title="Number of real products containing this ingredient")
        st.plotly_chart(fig, use_container_width=True)
    else:
        fig = px.treemap(filtered_ing, path=["category", "ingredient"], values="real_usage_count", color="category", height=600)
        st.plotly_chart(fig, use_container_width=True)
    zero = filtered_ing[filtered_ing["real_usage_count"] == 0]["ingredient"].tolist()
    if zero:
        st.caption(f"⚠️ Not found in this product dataset: {', '.join(zero)} (genuine data gap, not an error).")

# ================= TAB 5: Scatter =================
with tabs[4]:
    st.subheader("pH Sensitivity vs. Irritation Potential")
    st.caption("Curated scores (1-5), not from the product dataset.")
    if filtered_ing.empty:
        st.info("No ingredients match the current filters.")
    else:
        fig = px.scatter(filtered_ing, x="ph_sensitivity", y="irritation_potential", size="real_usage_count",
                          color="category", hover_name="ingredient", size_max=40, height=600)
        fig.update_layout(xaxis_title="pH sensitivity (1=stable, 5=highly pH-dependent)",
                           yaxis_title="Irritation potential (1=low, 5=high)")
        st.plotly_chart(fig, use_container_width=True)

# ================= TAB 6: Compatibility Checker + Recommendations =================
with tabs[5]:
    st.subheader("Check Ingredient Compatibility & Find Real Products")
    concern = st.selectbox("Your skin concern", sorted(ing["concern"].unique()))
    concern_ings = ing[ing["concern"] == concern]["ingredient"].tolist()
    chosen = st.multiselect("Ingredients you're considering", concern_ings, default=concern_ings[:2] if len(concern_ings) >= 2 else concern_ings)

    if len(chosen) >= 2:
        st.write("**Compatibility between selected ingredients:**")
        for i in range(len(chosen)):
            for j in range(i + 1, len(chosen)):
                a, b = chosen[i], chosen[j]
                match = rules[
                    ((rules["ingredient_a"] == a) & (rules["ingredient_b"] == b)) |
                    ((rules["ingredient_a"] == b) & (rules["ingredient_b"] == a))
                ]
                if len(match):
                    row = match.iloc[0]
                    icon = {"synergy": "🟢", "caution": "🟡", "avoid": "🔴"}[row["status"]]
                    st.write(f"{icon} **{a} + {b}** — {row['status'].upper()}: {row['reason']}")
                else:
                    st.write(f"⚪ **{a} + {b}** — no known rule on file (not confirmed safe or unsafe).")

    st.divider()
    st.write("**Real products containing your selected ingredients:**")
    for name in chosen:
        alias = ing[ing["ingredient"] == name].iloc[0]["inci_alias"]
        results = find_products(alias)
        with st.expander(f"{name} — {len(results)} product(s) shown"):
            if results.empty:
                st.write("No products in this dataset contain this ingredient.")
            else:
                for _, r in results.iterrows():
                    st.markdown(f"- [{r['product_name']}]({r['product_url']}) — {r['product_type']} · £{r['price_gbp']:.2f}")

st.divider()
with st.expander("ℹ️ About this dashboard / data sources"):
    st.markdown("""
    **Real data:** Product names, prices, and full ingredient lists come from a public
    Kaggle dataset of 1,138 skincare products (author: eward96), scraped from a UK
    retailer. Usage-frequency counts on the "Real Usage Frequency" tab are computed
    directly from this dataset, not estimated.

    **Real data:** Ingredient function descriptions come from a public Kaggle
    ingredient-function dataset (248 ingredients). 16 of our 30 tracked ingredients
    matched by name; the rest have no real source description available.

    **Curated data (clearly separate):** Skin-type fit scores, pH sensitivity,
    irritation potential, and the ingredient compatibility rules (network graph /
    heatmap / checker) are compiled by the project author from established
    dermatological guidance — no public dataset of ingredient *interactions* exists,
    so this layer is documented here rather than mislabeled as sourced data.

    This is an educational tool, not medical advice.
    """)
