library(shiny)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(plotly)
library(igraph)

# ---------- Data loading ----------

parse_ingredient_list <- function(x) {
  x <- str_remove(x, "^\\[")
  x <- str_remove(x, "\\]$")
  items <- str_split(x, "',\\s*'")[[1]]
  items <- str_remove(items, "^'")
  items <- str_remove(items, "'$")
  trimws(items)
}

ing      <- read_csv("data/ingredients_master_enriched.csv", show_col_types = FALSE)
rules    <- read_csv("data/compatibility_rules.csv", show_col_types = FALSE)
products <- read_csv("data/products_clean.csv", show_col_types = FALSE)
products$ingredient_list <- map(products$ingredient_list, parse_ingredient_list)

STATUS_COLORS <- c(synergy = "#2E8B57", caution = "#E8A33D", avoid = "#C0392B")

find_products <- function(alias, products, top_n = 6) {
  alias <- tolower(alias)
  mask <- map_lgl(products$ingredient_list, function(lst) {
    any(str_detect(tolower(trimws(lst)), fixed(alias)))
  })
  head(products[mask, ], top_n)
}

empty_plot_msg <- function(msg) {
  plot_ly() %>%
    layout(title = msg,
           xaxis = list(visible = FALSE),
           yaxis = list(visible = FALSE))
}

# ---------- UI ----------

ui <- fluidPage(
  title = "Skincare Ingredient Explorer",
  titlePanel("\U0001F9F4 Interactive Skincare Ingredient & Compatibility Explorer"),
  tags$p(
    "Real product & ingredient data from a public Kaggle dataset (1,138 products), ",
    "enriched with real ingredient function data where available. Skin-type fit and ",
    "compatibility rules are a clearly-labeled researcher-curated layer \u2014 see About section at the bottom."
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectizeInput("concern_filter", "Filter by concern",
                     choices = sort(unique(ing$concern)), multiple = TRUE),
      selectizeInput("category_filter", "Filter by category",
                     choices = sort(unique(ing$category)), multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Network Graph",
                 plotlyOutput("network_plot", height = "650px"),
                 tags$p("\U0001F7E2 synergy \u00B7 \U0001F7E1 caution \u00B7 \U0001F534 avoid \u2014 hover for the reason. (Curated relationship data.)")),
        tabPanel("Compatibility Heatmap",
                 plotlyOutput("heatmap_plot", height = "650px"),
                 tags$p("Cells with no known rule show as neutral \u2014 that means 'no data', not 'confirmed safe'.")),
        tabPanel("Skin-Type Fit (Radar)",
                 tags$p("Curated scores (1-5), not from the product dataset."),
                 selectizeInput("radar_select", "Select ingredients to compare",
                                choices = NULL, multiple = TRUE),
                 plotlyOutput("radar_plot", height = "600px")),
        tabPanel("Real Usage Frequency",
                 uiOutput("usage_caption"),
                 radioButtons("usage_view", "View as", choices = c("Bar chart", "Treemap"), inline = TRUE),
                 plotlyOutput("usage_plot", height = "650px"),
                 uiOutput("zero_usage_note")),
        tabPanel("pH vs Irritation (Scatter)",
                 tags$p("Curated scores (1-5), not from the product dataset."),
                 plotlyOutput("scatter_plot", height = "600px")),
        tabPanel("Compatibility Checker + Recommendations",
                 selectInput("concern_select", "Your skin concern", choices = sort(unique(ing$concern))),
                 selectizeInput("chosen_ingredients", "Ingredients you're considering",
                                choices = NULL, multiple = TRUE),
                 tags$h4("Compatibility between selected ingredients:"),
                 uiOutput("compat_results"),
                 tags$hr(),
                 tags$h4("Real products containing your selected ingredients:"),
                 uiOutput("product_results"))
      ),
      tags$hr(),
      tags$details(
        tags$summary("\u2139\uFE0F About this dashboard / data sources"),
        tags$div(
          tags$p(tags$b("Real data:"), " Product names, prices, and full ingredient lists come from a public Kaggle dataset of 1,138 skincare products (author: eward96), scraped from a UK retailer. Usage-frequency counts on the 'Real Usage Frequency' tab are computed directly from this dataset, not estimated."),
          tags$p(tags$b("Real data:"), " Ingredient function descriptions come from a public Kaggle ingredient-function dataset (248 ingredients). 16 of our 30 tracked ingredients matched by name; the rest have no real source description available."),
          tags$p(tags$b("Curated data (clearly separate):"), " Skin-type fit scores, pH sensitivity, irritation potential, and the ingredient compatibility rules (network graph / heatmap / checker) are compiled by the project author from established dermatological guidance \u2014 no public dataset of ingredient interactions exists, so this layer is documented here rather than mislabeled as sourced data."),
          tags$p(tags$i("This is an educational tool, not medical advice."))
        )
      )
    )
  )
)

# ---------- Server ----------

server <- function(input, output, session) {
  
  filtered_ing <- reactive({
    d <- ing
    if (!is.null(input$concern_filter) && length(input$concern_filter) > 0) {
      d <- d %>% filter(concern %in% input$concern_filter)
    }
    if (!is.null(input$category_filter) && length(input$category_filter) > 0) {
      d <- d %>% filter(category %in% input$category_filter)
    }
    d
  })
  
  filtered_names <- reactive({
    unique(filtered_ing()$ingredient)
  })
  
  filtered_rules <- reactive({
    rules %>% filter(ingredient_a %in% filtered_names() & ingredient_b %in% filtered_names())
  })
  
  observe({
    opts <- sort(unique(filtered_ing()$ingredient))
    default_sel <- if (length(opts) >= 4) opts[1:4] else opts
    updateSelectizeInput(session, "radar_select", choices = opts, selected = default_sel)
  })
  
  observeEvent(input$concern_select, {
    concern_ings <- ing %>% filter(concern == input$concern_select) %>% pull(ingredient)
    default_sel <- if (length(concern_ings) >= 2) concern_ings[1:2] else concern_ings
    updateSelectizeInput(session, "chosen_ingredients", choices = concern_ings, selected = default_sel)
  }, ignoreNULL = FALSE)
  
  output$usage_caption <- renderUI({
    tags$p(paste0("Counted directly from the ", nrow(products), "-product dataset \u2014 not estimated."))
  })
  
  # ---------- Tab 1: Network Graph ----------
  output$network_plot <- renderPlotly({
    fi <- filtered_ing()
    fr <- filtered_rules()
    
    if (nrow(fi) == 0) return(empty_plot_msg("No ingredients match the current filters."))
    
    if (nrow(fr) > 0) {
      g <- graph_from_data_frame(
        d = fr %>% select(ingredient_a, ingredient_b, status, reason),
        vertices = fi %>% select(ingredient, category),
        directed = FALSE
      )
    } else {
      g <- make_empty_graph(n = nrow(fi), directed = FALSE)
      V(g)$name <- fi$ingredient
      V(g)$category <- fi$category
    }
    
    set.seed(42)
    layout_coords <- layout_with_fr(g)
    pos <- data.frame(name = V(g)$name, x = layout_coords[, 1], y = layout_coords[, 2])
    
    fig <- plot_ly()
    
    for (st in names(STATUS_COLORS)) {
      edges_st <- if (nrow(fr) > 0) fr %>% filter(status == st) else fr[0, ]
      ex <- c(); ey <- c()
      if (nrow(edges_st) > 0) {
        for (k in seq_len(nrow(edges_st))) {
          a <- edges_st$ingredient_a[k]; b <- edges_st$ingredient_b[k]
          x0 <- pos$x[pos$name == a]; y0 <- pos$y[pos$name == a]
          x1 <- pos$x[pos$name == b]; y1 <- pos$y[pos$name == b]
          ex <- c(ex, x0, x1, NA); ey <- c(ey, y0, y1, NA)
        }
      }
      fig <- fig %>% add_trace(
        x = ex, y = ey, type = "scatter", mode = "lines",
        line = list(width = 2, color = STATUS_COLORS[[st]]),
        name = st, hoverinfo = "skip"
      )
    }
    
    degrees <- degree(g)
    fig <- fig %>% add_trace(
      x = pos$x, y = pos$y, type = "scatter", mode = "markers+text",
      text = pos$name, textposition = "top center",
      textfont = list(size = 9),
      hovertext = paste0(pos$name, " \u2014 ", degrees[pos$name], " connections"),
      hoverinfo = "text",
      marker = list(size = 12 + degrees[pos$name] * 3, color = "#4A6FA5",
                    line = list(width = 1, color = "white")),
      name = "ingredient"
    )
    
    fig %>% layout(
      showlegend = TRUE, height = 650,
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      margin = list(l = 10, r = 10, t = 10, b = 10)
    )
  })
  
  # ---------- Tab 2: Heatmap ----------
  output$heatmap_plot <- renderPlotly({
    names_v <- sort(filtered_names())
    fr <- filtered_rules()
    
    if (length(names_v) == 0) return(empty_plot_msg("No ingredients match the current filters."))
    
    status_val <- c(avoid = -1, caution = 0, synergy = 1)
    m <- matrix(0, nrow = length(names_v), ncol = length(names_v),
                dimnames = list(names_v, names_v))
    
    if (nrow(fr) > 0) {
      for (k in seq_len(nrow(fr))) {
        a <- fr$ingredient_a[k]; b <- fr$ingredient_b[k]; s <- fr$status[k]
        if (a %in% names_v && b %in% names_v) {
          m[a, b] <- status_val[[s]]
          m[b, a] <- status_val[[s]]
        }
      }
    }
    
    plot_ly(
      z = m, x = names_v, y = names_v, type = "heatmap",
      colorscale = list(list(0, "#C0392B"), list(0.5, "#E8A33D"), list(1, "#2E8B57")),
      zmin = -1, zmax = 1,
      colorbar = list(title = "Status", tickvals = c(-1, 0, 1), ticktext = c("Avoid", "Caution", "Synergy"))
    ) %>% layout(height = 650)
  })
  
  # ---------- Tab 3: Radar ----------
  output$radar_plot <- renderPlotly({
    selected <- input$radar_select
    if (is.null(selected) || length(selected) == 0) return(NULL)
    
    categories <- c("oily", "dry", "combination", "sensitive")
    fig <- plot_ly(type = "scatterpolar", fill = "toself")
    
    for (nm in selected) {
      row <- ing %>% filter(ingredient == nm) %>% slice(1)
      values <- c(as.numeric(row[categories]), as.numeric(row[categories[1]]))
      theta <- c(categories, categories[1])
      fig <- fig %>% add_trace(r = values, theta = theta, name = nm)
    }
    
    fig %>% layout(
      polar = list(radialaxis = list(visible = TRUE, range = c(0, 5))),
      height = 600, showlegend = TRUE
    )
  })
  
  # ---------- Tab 4: Usage frequency ----------
  output$usage_plot <- renderPlotly({
    plot_df <- filtered_ing() %>% arrange(real_usage_count)
    
    if (nrow(plot_df) == 0) return(empty_plot_msg("No ingredients match the current filters."))
    
    if (input$usage_view == "Bar chart") {
      plot_ly(plot_df, x = ~real_usage_count, y = ~factor(ingredient, levels = ingredient),
              color = ~category, type = "bar", orientation = "h") %>%
        layout(yaxis = list(title = ""),
               xaxis = list(title = "Number of real products containing this ingredient"),
               height = max(400, nrow(plot_df) * 22))
    } else {
      cats <- unique(plot_df$category)
      cat_values <- sapply(cats, function(c) sum(plot_df$real_usage_count[plot_df$category == c]))
      labels <- c(cats, plot_df$ingredient)
      parents <- c(rep("", length(cats)), plot_df$category)
      values <- c(cat_values, plot_df$real_usage_count)
      plot_ly(type = "treemap", labels = labels, parents = parents, values = values,
              branchvalues = "total") %>%
        layout(height = 600)
    }
  })
  
  output$zero_usage_note <- renderUI({
    zero <- filtered_ing() %>% filter(real_usage_count == 0) %>% pull(ingredient)
    if (length(zero) > 0) {
      tags$p(paste0("\u26A0\uFE0F Not found in this product dataset: ", paste(zero, collapse = ", "),
                    " (genuine data gap, not an error)."))
    }
  })
  
  # ---------- Tab 5: Scatter ----------
  output$scatter_plot <- renderPlotly({
    fi <- filtered_ing()
    if (nrow(fi) == 0) return(empty_plot_msg("No ingredients match the current filters."))
    
    plot_ly(
      fi, x = ~ph_sensitivity, y = ~irritation_potential,
      size = ~real_usage_count, color = ~category,
      text = ~ingredient, hoverinfo = "text",
      type = "scatter", mode = "markers",
      sizes = c(5, 40)
    ) %>% layout(
      xaxis = list(title = "pH sensitivity (1=stable, 5=highly pH-dependent)"),
      yaxis = list(title = "Irritation potential (1=low, 5=high)"),
      height = 600
    )
  })
  
  # ---------- Tab 6: Compatibility checker + recommendations ----------
  output$compat_results <- renderUI({
    chosen <- input$chosen_ingredients
    if (is.null(chosen) || length(chosen) < 2) {
      return(tags$p("Select at least two ingredients above to compare."))
    }
    
    rows <- list()
    for (i in seq_len(length(chosen) - 1)) {
      for (j in seq(i + 1, length(chosen))) {
        a <- chosen[i]; b <- chosen[j]
        match <- rules %>% filter(
          (ingredient_a == a & ingredient_b == b) | (ingredient_a == b & ingredient_b == a)
        )
        if (nrow(match) > 0) {
          st <- match$status[1]
          icon <- switch(st, synergy = "\U0001F7E2", caution = "\U0001F7E1", avoid = "\U0001F534")
          rows[[length(rows) + 1]] <- tags$p(
            HTML(paste0(icon, " <b>", a, " + ", b, "</b> \u2014 ", toupper(st), ": ", match$reason[1]))
          )
        } else {
          rows[[length(rows) + 1]] <- tags$p(
            HTML(paste0("\u26AA <b>", a, " + ", b, "</b> \u2014 no known rule on file (not confirmed safe or unsafe)."))
          )
        }
      }
    }
    do.call(tagList, rows)
  })
  
  output$product_results <- renderUI({
    chosen <- input$chosen_ingredients
    if (is.null(chosen) || length(chosen) == 0) return(NULL)
    
    blocks <- list()
    for (nm in chosen) {
      alias <- ing %>% filter(ingredient == nm) %>% pull(inci_alias) %>% .[1]
      results <- find_products(alias, products)
      
      body <- if (nrow(results) == 0) {
        tags$p("No products in this dataset contain this ingredient.")
      } else {
        tags$ul(lapply(seq_len(nrow(results)), function(k) {
          r <- results[k, ]
          tags$li(tags$a(href = r$product_url, target = "_blank", r$product_name),
                  paste0(" \u2014 ", r$product_type, " \u00B7 \u00A3", sprintf("%.2f", r$price_gbp)))
        }))
      }
      
      blocks[[length(blocks) + 1]] <- tags$details(
        tags$summary(paste0(nm, " \u2014 ", nrow(results), " product(s) shown")),
        body
      )
    }
    tagList(blocks)
  })
}

shinyApp(ui, server)