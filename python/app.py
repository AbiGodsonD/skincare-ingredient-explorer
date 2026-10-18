import streamlit as st
import pandas as pd
import networkx as nx
import plotly.graph_objects as go
import plotly.express as px

st.set_page_config(page_title="Skincare Ingredient Explorer", layout="wide")

# ---------- Data loading ----------
@st.cache_data
def load_data():
    ing = pd.read_csv("data/ingredients.csv")
    rules = pd.read_csv("data/compatibility_rules.csv")
    return ing, rules

ing, rules = load_data()

STATUS_COLORS = {"synergy": "#2E8B57", "caution": "#E8A33D", "avoid": "#C0392B"}

st.title("🧴 Interactive Skincare Ingredient & Compatibility Explorer")
st.caption(
    "Explore how common skincare actives interact. Compatibility rules are hand-curated "
    "from established dermatological guidance (see About tab for sources)."
)

tabs = st.tabs([
    "🕸️ Network Graph",
    "🔥 Compatibility Heatmap",
    "🎯 Skin-Type Fit (Radar)",
    "📊 Ingredient Frequency",
    "🌊 Concern → Ingredient Flow",
    "⚗️ pH vs Irritation (Scatter)",
])

# ---------- Sidebar filters (apply across tabs) ----------
st.sidebar.header("Filters")
concern_filter = st.sidebar.multiselect(
    "Filter by concern", sorted(ing["concern"].unique()), default=[]
)
category_filter = st.sidebar.multiselect(
    "Filter by category", sorted(ing["category"].unique()), default=[]
)

filtered_ing = ing.copy()
if concern_filter:
    filtered_ing = filtered_ing[filtered_ing["concern"].isin(concern_filter)]
if category_filter:
    filtered_ing = filtered_ing[filtered_ing["category"].isin(category_filter)]

filtered_names = set(filtered_ing["ingredient"])
filtered_rules = rules[
    rules["ingredient_a"].isin(filtered_names) & rules["ingredient_b"].isin(filtered_names)
]

# ================= TAB 1: Network Graph =================
with tabs[0]:
    st.subheader("Ingredient Compatibility Network")
    st.write(
        "Nodes are ingredients; edges show a known relationship. "
        "🟢 synergy · 🟡 caution · 🔴 avoid. Hover over edges for the reason."
    )

    G = nx.Graph()
    for _, row in filtered_ing.iterrows():
        G.add_node(row["ingredient"], category=row["category"])
    for _, row in filtered_rules.iterrows():
        G.add_edge(row["ingredient_a"], row["ingredient_b"],
                    status=row["status"], reason=row["reason"])

    if G.number_of_nodes() == 0:
        st.info("No ingredients match the current filters.")
    else:
        pos = nx.spring_layout(G, seed=42, k=0.8)

        edge_traces = []
        for status, color in STATUS_COLORS.items():
            ex, ey = [], []
            hover_texts = []
            for u, v, data in G.edges(data=True):
                if data["status"] != status:
                    continue
                x0, y0 = pos[u]
                x1, y1 = pos[v]
                ex += [x0, x1, None]
                ey += [y0, y1, None]
            edge_traces.append(
                go.Scatter(x=ex, y=ey, mode="lines",
                            line=dict(width=2, color=color),
                            name=status, hoverinfo="skip")
            )

        node_x = [pos[n][0] for n in G.nodes()]
        node_y = [pos[n][1] for n in G.nodes()]
        node_text = [f"{n} ({G.nodes[n]['category']})<br>Connections: {G.degree(n)}" for n in G.nodes()]
        degrees = [G.degree(n) for n in G.nodes()]

        node_trace = go.Scatter(
            x=node_x, y=node_y, mode="markers+text",
            text=[n for n in G.nodes()],
            textposition="top center",
            textfont=dict(size=9),
            hovertext=node_text, hoverinfo="text",
            marker=dict(
                size=[12 + d * 3 for d in degrees],
                color="#4A6FA5", line=dict(width=1, color="white")
            ),
            name="ingredient"
        )

        fig = go.Figure(data=edge_traces + [node_trace])
        fig.update_layout(
            showlegend=True, height=650,
            xaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
            yaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
            margin=dict(l=10, r=10, t=10, b=10),
        )
        st.plotly_chart(fig, use_container_width=True)

        with st.expander("Ingredient with most conflicts/interactions"):
            deg_df = pd.DataFrame(
                [(n, G.degree(n)) for n in G.nodes()], columns=["Ingredient", "Connections"]
            ).sort_values("Connections", ascending=False)
            st.dataframe(deg_df, use_container_width=True, hide_index=True)

# ================= TAB 2: Heatmap =================
with tabs[1]:
    st.subheader("Ingredient × Ingredient Compatibility Matrix")
    names = sorted(filtered_names)
    status_val = {"avoid": -1, "caution": 0, "synergy": 1}
    matrix = pd.DataFrame(0, index=names, columns=names, dtype=float)

    for _, row in filtered_rules.iterrows():
        a, b, s = row["ingredient_a"], row["ingredient_b"], row["status"]
        if a in names and b in names:
            matrix.loc[a, b] = status_val[s]
            matrix.loc[b, a] = status_val[s]

    if len(names) == 0:
        st.info("No ingredients match the current filters.")
    else:
        fig = px.imshow(
            matrix, color_continuous_scale=["#C0392B", "#E8A33D", "#2E8B57"],
            zmin=-1, zmax=1, aspect="auto"
        )
        fig.update_layout(height=650, coloraxis_colorbar=dict(
            title="Status", tickvals=[-1, 0, 1], ticktext=["Avoid", "Caution", "Synergy"]
        ))
        st.plotly_chart(fig, use_container_width=True)
        st.caption("Cells with no known rule are shown as neutral (mid-color), not necessarily safe.")

# ================= TAB 3: Radar =================
with tabs[2]:
    st.subheader("Skin-Type Fit by Ingredient")
    options = sorted(filtered_ing["ingredient"].unique())
    default_sel = options[:4] if len(options) >= 4 else options
    selected = st.multiselect("Select ingredients to compare", options, default=default_sel)

    if selected:
        categories = ["oily", "dry", "combination", "sensitive"]
        fig = go.Figure()
        for name in selected:
            row = ing[ing["ingredient"] == name].iloc[0]
            values = [row[c] for c in categories] + [row[categories[0]]]
            fig.add_trace(go.Scatterpolar(
                r=values, theta=categories + [categories[0]],
                fill="toself", name=name
            ))
        fig.update_layout(
            polar=dict(radialaxis=dict(visible=True, range=[0, 5])),
            height=600, showlegend=True
        )
        st.plotly_chart(fig, use_container_width=True)
        st.caption("Scale: 1 (poor fit) to 5 (excellent fit) for each skin type.")
    else:
        st.info("Select at least one ingredient above.")

# ================= TAB 4: Frequency bar/treemap =================
with tabs[3]:
    st.subheader("Ingredient Usage Frequency by Category")
    view = st.radio("View as", ["Bar chart", "Treemap"], horizontal=True)

    if filtered_ing.empty:
        st.info("No ingredients match the current filters.")
    else:
        if view == "Bar chart":
            plot_df = filtered_ing.sort_values("usage_frequency", ascending=True)
            fig = px.bar(
                plot_df, x="usage_frequency", y="ingredient", color="category",
                orientation="h", height=max(400, len(plot_df) * 22)
            )
            fig.update_layout(yaxis_title="", xaxis_title="Usage frequency (products)")
        else:
            fig = px.treemap(
                filtered_ing, path=["category", "ingredient"], values="usage_frequency",
                color="category", height=600
            )
        st.plotly_chart(fig, use_container_width=True)

# ================= TAB 5: Sankey =================
with tabs[4]:
    st.subheader("Concern → Category → Ingredient Flow")
    sankey_df = filtered_ing.copy()
    if sankey_df.empty:
        st.info("No ingredients match the current filters.")
    else:
        concerns = sankey_df["concern"].unique().tolist()
        categories = sankey_df["category"].unique().tolist()
        ingredients_list = sankey_df["ingredient"].tolist()

        labels = concerns + categories + ingredients_list
        label_idx = {l: i for i, l in enumerate(labels)}

        src, tgt, val = [], [], []
        # concern -> category
        cc = sankey_df.groupby(["concern", "category"]).size().reset_index(name="count")
        for _, r in cc.iterrows():
            src.append(label_idx[r["concern"]])
            tgt.append(label_idx[r["category"]])
            val.append(r["count"])
        # category -> ingredient
        for _, r in sankey_df.iterrows():
            src.append(label_idx[r["category"]])
            tgt.append(label_idx[r["ingredient"]])
            val.append(1)

        fig = go.Figure(go.Sankey(
            node=dict(label=labels, pad=12, thickness=14),
            link=dict(source=src, target=tgt, value=val)
        ))
        fig.update_layout(height=max(500, len(ingredients_list) * 18))
        st.plotly_chart(fig, use_container_width=True)

# ================= TAB 6: Scatter =================
with tabs[5]:
    st.subheader("pH Sensitivity vs. Irritation Potential")
    if filtered_ing.empty:
        st.info("No ingredients match the current filters.")
    else:
        fig = px.scatter(
            filtered_ing, x="ph_sensitivity", y="irritation_potential",
            size="usage_frequency", color="category", hover_name="ingredient",
            size_max=40, height=600
        )
        fig.update_layout(
            xaxis_title="pH sensitivity (1=stable, 5=highly pH-dependent)",
            yaxis_title="Irritation potential (1=low, 5=high)"
        )
        st.plotly_chart(fig, use_container_width=True)

st.divider()
with st.expander("ℹ️ About this dashboard / data sources"):
    st.markdown("""
    **Ingredient dataset:** Compiled from common skincare formulation references and
    public ingredient datasets (e.g. Kaggle skincare/INCI datasets), then curated down
    to 30 well-known actives for clarity.

    **Compatibility rules:** Hand-curated from established, widely-cited dermatological
    guidance (e.g. retinoid-exfoliant conflicts, vitamin C stability requirements,
    benzoyl peroxide oxidation effects). This is a simplified educational model, not
    medical advice — always patch-test new combinations and consult a dermatologist
    for personalized guidance.
    """)
