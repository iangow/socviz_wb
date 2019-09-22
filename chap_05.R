library(ggplot2)

# 5.1: Use pipes to summarize data ----
rel_by_region <-
    socviz::gss_sm %>%
    group_by(bigregion, religion) %>%
    summarise(N = n()) %>%
    mutate(freq = N/ sum(N),
           pct = round(freq* 100, 1))

rel_by_region

rel_by_region %>%
    group_by(bigregion) %>%
    summarise(total = sum(pct))

p <- ggplot(rel_by_region, aes(x = religion, y = pct, fill = religion)) 
p + geom_col(position = "dodge2") + 
    labs(x = NULL, y = "Percent", fill = "Religion") + 
    guides(fill = FALSE) +
    coord_flip() +
    facet_grid(~ bigregion)

# 5.2: Continuous variables by group or category ----
organdata <- socviz::organdata

p <- ggplot(organdata, aes(x = year, y = donors))
p + geom_point()

p + geom_line(aes(group = country)) + facet_wrap(~ country)

p <- ggplot(data = plot_data, mapping = aes(x = gdpAdj, y = lifeExpAdj))
p + geom_point(mapping = aes(color = continent)) +
    geom_smooth(method = "loess") +
    scale_x_log10()

p + geom_smooth(method = "loess") +
    geom_point(mapping = aes(color = continent)) +
    scale_x_log10()

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp, color = factor(year)))
p + geom_point(mapping = aes(color = factor(year))) +
    geom_smooth(method = "loess", se = FALSE) +
    scale_x_log10() 

organdata %>% select(1:6) %>% sample_n(size = 10)

p <- ggplot(organdata, mapping = aes(x = year, y = donors))
p + geom_point()

p + geom_line(aes(group = country)) + facet_wrap(~ country)

p <- ggplot(organdata, mapping = aes(x = country, y = donors))
p + geom_boxplot() + coord_flip()

p <- ggplot(organdata,
            mapping = aes(x = reorder(country, donors, na.rm = TRUE),
                          y = donors))

organdata %>%
    mutate(country = fct_reorder(country, donors, median, na.rm = TRUE)) %>%
    ggplot(aes(x = country, y = donors)) +
    geom_boxplot() + labs(x = NULL) + coord_flip()

p + geom_violin() + 
    labs(x = NULL) +
    coord_flip()

organdata %>%
    mutate(country = fct_reorder(country, donors, median, na.rm = TRUE)) %>%
    ggplot(aes(x = country, y = donors, color = world)) +
    geom_point() + labs(x = NULL) + coord_flip() +
    theme(legend.position = "top")

organdata %>%
    mutate(country = fct_reorder(country, donors, median, na.rm = TRUE)) %>%
    ggplot(aes(x = country, y = donors, color = world)) +
    geom_jitter() + labs(x = NULL) + coord_flip() +
    theme(legend.position = "top")

organdata %>%
    mutate(country = fct_reorder(country, donors, median, na.rm = TRUE)) %>%
    ggplot(aes(x = country, y = donors, color = world)) +
    geom_jitter(position = position_jitter(width =0.15)) + 
    labs(x = NULL) + coord_flip() +
    theme(legend.position = "top")

by_country <-
    organdata %>%
    group_by(consent_law, country) %>%
    summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>%
    ungroup()

by_country

# 5.3 Plot text directly ----
library(socviz)
p <- ggplot(data = by_country, aes(x = roads_mean, y = donors_mean))
p + geom_point() + geom_text(aes(label = country))

p + geom_point() + geom_text(aes(label = country), hjust = 0)

library(ggrepel)
elections_historic %>% select(2:7)

p_title <- "Presidential Elections: Popular & Electoral College Margins"
p_subtitle <- "1824-2016"
p_caption <- "Data for 2016 are provisional."
x_label <- "Winner's share of Popular Vote"
y_label <- "Winner's share of Electoral College Votes"

p <- ggplot(elections_historic, aes(x = popular_pct, y = ec_pct,
                                    label = winner_label))

p + 
    geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
    geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = x_label, y = y_label, title = p_title, subtitle = p_subtitle,
         caption = p_caption)

# 5.4: Label outliers

p <- ggplot(data = by_country,
            mapping = aes(x = gdp_mean, y = health_mean))

p + geom_point() +
    geom_text_repel(data = subset(by_country, gdp_mean > 25000),
                    mapping = aes(label = country))

p <- ggplot(data = by_country,
            mapping = aes(x = gdp_mean, y = health_mean))

p + geom_point() +
    geom_text_repel(data = subset(by_country,
                                  gdp_mean > 25000 | health_mean < 1500 |
                                  country %in% "Belgium"),
                    aes(label = country))
p <- ggplot(data = by_country,
            mapping = aes(x = gdp_mean, y = health_mean))

p + geom_point() +
    geom_text_repel(data = subset(by_country, gdp_mean > 25000),
                    mapping = aes(label = country))

organdata <-
    organdata %>%
    mutate(ind = ccode %in% c("Ita", "Spa") & year > 1998) 
p <- ggplot(organdata, mapping = aes(x = roads,
                          y = donors, color = ind))
p + 
    geom_point() +
    geom_text_repel(filter(organdata, ind),
                    mapping = aes(label = ccode)) +
                    guides(label = FALSE, color = FALSE)
p                    