format_tbl_tb <- function(tbl_obj){
  tbl_obj %>%
    bold_p(q = TRUE) %>%
    bold_labels() %>%
    as_gt() %>%
    tab_options(
      table.font.size = "13px",
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold",
      table.border.top.color = "white",
      heading.border.bottom.color = "black",
      table.border.bottom.color = "white",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table_body.hlines.color = "white",
      container.overflow.x = T,
      container.overflow.y = T
    ) %>%
    opt_align_table_header(align = "left")
}


format_pairwise_wilcoxon_df <- function(df_obj){
  df_obj %>%
    gt() %>%
    tab_options(
      table.font.size = "13px",
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold",
      table.border.top.color = "white",
      heading.border.bottom.color = "black",
      table.border.bottom.color = "white",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table_body.hlines.color = "white",
      container.overflow.x = T,
      container.overflow.y = T
    ) %>%
    opt_align_table_header(align = "left") %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = `Term`,
        rows = `CO vs. FO` < 0.05 | `LA vs. FO` < 0.05 | `LA vs. CO` < 0.05
      )) %>%
    tab_style(
      style = list(cell_fill(color = "#F8766D", alpha = 0.5)),
      locations = cells_body(columns = "CO vs. FO",
                             rows = `CO vs. FO` < 0.05)) %>%
    tab_style(
      style = list(cell_fill(color = "#F8766D", alpha = 0.5)),
      locations = cells_body(columns = "LA vs. FO",
                             rows = `LA vs. FO` < 0.05)) %>%
    tab_style(
      style = list(cell_fill(color = "#F8766D", alpha = 0.5)),
      locations = cells_body(columns = "LA vs. CO",
                             rows = `LA vs. CO` < 0.05))
}

format_df_tb <- function(df_obj){
  df_obj %>%
    gt() %>%
    tab_options(
      table.font.size = "13px",
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold",
      table.border.top.color = "white",
      heading.border.bottom.color = "black",
      table.border.bottom.color = "white",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table_body.hlines.color = "white",
      container.overflow.x = T,
      container.overflow.y = T
    ) %>%
    opt_align_table_header(align = "left")
}

format_df_lm_table <- function(df_obj){
  df_obj %>%
    # only show term with at least one significant beta (coefficient)
    filter(p1 < 0.05 | p2 < 0.05 | p3 < 0.05 | p4 < 0.05) %>%
    # create a column used for table format (color and bold) - keep the original p value
    mutate(p1color = p1,
           p2color = p2,
           p3color = p3,
           p4color = p4) %>%
    # format p value - "<0.001"
    mutate(p1 = if_else(p1 < 0.001,
                        0,
                        p1),
           p2 = if_else(p2 < 0.001,
                        0,
                        p2),
           p3 = if_else(p3 < 0.001,
                        0,
                        p3),
           p4 = if_else(p4 < 0.001,
                        0,
                        p4)) %>%
    mutate_if(is.numeric, round, 3) %>%
    mutate(p1 = ifelse(p1 == 0,
                       "<0.001",
                       p1),
           p2 = ifelse(p2 == 0,
                       "<0.001",
                       p2),
           p3 = ifelse(p3 == 0,
                       "<0.001",
                       p3),
           p4 = ifelse(p4 == 0,
                       "<0.001",
                       p4)) %>%
    # convert table into gt table object
    gt() %>%
    # general format of gt table
    tab_options(
      table.font.size = "13px",
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold",
      table.border.top.color = "white",
      heading.border.bottom.color = "black",
      table.border.bottom.color = "white",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table_body.hlines.color = "white",
      container.overflow.x = T,
      container.overflow.y = T
    ) %>%
    opt_align_table_header(align = "left") %>%
    # bold pvalue < 0.05
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = p1,
        rows = p1color < 0.05
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = p2,
        rows = p2color < 0.05
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = p3,
        rows = p3color < 0.05
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = p4,
        rows = p4color < 0.05
      )
    ) %>%
    # color pvalue < 0.05
    tab_style(
      style = list(cell_fill(color = "#e0ecf4")),
      locations = cells_body(columns = p1,
                             rows = p1color < 0.05)) %>%
    tab_style(
      style = list(cell_fill(color = "#dbedd3")),##afc9a3")),
      locations = cells_body(columns = p2,
                             rows = p2color < 0.05)) %>%
    tab_style(
      style = list(cell_fill(color = "#fad282")),
      locations = cells_body(columns = p3,
                             rows = p3color < 0.05)) %>%
    tab_style(
      style = list(cell_fill(color = "#fcb5a7")),
      locations = cells_body(columns = p4,
                             rows = p4color < 0.05)) %>%
    # hide tool column (no need to display in the report)
    cols_hide(columns = c(p1color, p2color, p3color, p4color))
}


format_permanova_table <- function(df_obj){
  df_obj %>%
    # convert table into gt table object
    gt() %>%
    # general format of gt table
    tab_options(
      table.font.size = "13px",
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold",
      table.border.top.color = "white",
      heading.border.bottom.color = "black",
      table.border.bottom.color = "white",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table_body.hlines.color = "white",
      container.overflow.x = T,
      container.overflow.y = T
    ) %>%
    opt_align_table_header(align = "left") %>%
    # color pvalue < 0.05
    tab_style(
      style = list(cell_fill(color = "#e0ecf4")),
      locations = cells_body(columns = p.tumor,
                             rows = p.tumor < 0.05)) %>%
    tab_style(
      style = list(cell_fill(color = "#e0ecf4")),##afc9a3")),
      locations = cells_body(columns = p.diet,
                             rows = p.diet < 0.05)) %>%
    tab_style(
      style = list(cell_fill(color = "#e0ecf4")),
      locations = cells_body(columns = p.td,
                             rows = p.td < 0.05)) %>%
    tab_style(
      style = list(cell_fill(color = "#e0ecf4")),
      locations = cells_body(columns = p.dt,
                             rows = p.dt < 0.05)) %>%
    tab_style(
      style = list(cell_fill(color = "#e0ecf4")),
      locations = cells_body(columns =  p.inter,
                             rows = p.inter < 0.05))

}


format_df_lm_alpha_table <- function(df_obj){
  df_obj %>%
    # only show term with at least one significant beta (coefficient)
    # create a column used for table format (color and bold) - keep the original p value
    mutate(p1color = p1,
           p2color = p2,
           p3color = p3,
           p4color = p4) %>%
    # format p value - "<0.001"
    mutate(p1 = if_else(p1 < 0.001,
                        0,
                        p1),
           p2 = if_else(p2 < 0.001,
                        0,
                        p2),
           p3 = if_else(p3 < 0.001,
                        0,
                        p3),
           p4 = if_else(p4 < 0.001,
                        0,
                        p4)) %>%
    mutate_if(is.numeric, round, 3) %>%
    mutate(p1 = ifelse(p1 == 0,
                       "<0.001",
                       p1),
           p2 = ifelse(p2 == 0,
                       "<0.001",
                       p2),
           p3 = ifelse(p3 == 0,
                       "<0.001",
                       p3),
           p4 = ifelse(p4 == 0,
                       "<0.001",
                       p4)) %>%
    # convert table into gt table object
    gt() %>%
    # general format of gt table
    tab_options(
      table.font.size = "13px",
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold",
      table.border.top.color = "white",
      heading.border.bottom.color = "black",
      table.border.bottom.color = "white",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table_body.hlines.color = "white",
      container.overflow.x = T,
      container.overflow.y = T
    ) %>%
    opt_align_table_header(align = "left") %>%
    # bold pvalue < 0.05
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = p1,
        rows = p1color < 0.05
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = p2,
        rows = p2color < 0.05
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = p3,
        rows = p3color < 0.05
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = p4,
        rows = p4color < 0.05
      )
    ) %>%
    # color pvalue < 0.05
    tab_style(
      style = list(cell_fill(color = "#e0ecf4")),
      locations = cells_body(columns = p1,
                             rows = p1color < 0.05)) %>%
    tab_style(
      style = list(cell_fill(color = "#dbedd3")),##afc9a3")),
      locations = cells_body(columns = p2,
                             rows = p2color < 0.05)) %>%
    tab_style(
      style = list(cell_fill(color = "#fad282")),
      locations = cells_body(columns = p3,
                             rows = p3color < 0.05)) %>%
    tab_style(
      style = list(cell_fill(color = "#fcb5a7")),
      locations = cells_body(columns = p4,
                             rows = p4color < 0.05)) %>%
    # hide tool column (no need to display in the report)
    cols_hide(columns = c(p1color, p2color, p3color, p4color))
}

