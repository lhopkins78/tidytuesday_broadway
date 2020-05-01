library(tidyverse)

# Get the Data

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv')
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

cpi_adj <- cpi %>% mutate(cpi_adj=266.795/cpi) %>% rename(week_ending=year_month)

grosses_new <- grosses %>% inner_join(cpi_adj, by="week_ending") %>%
  mutate(real_weekly_gross_overall=weekly_gross_overall*cpi_adj, 
         real_weekly_gross=cpi_adj*weekly_gross,
         real_potential_gross=potential_gross*cpi_adj,
         real_avg_ticket_price=avg_ticket_price*cpi_adj,
         real_top_ticket_price=top_ticket_price*cpi_adj) %>%
  select(-cpi_adj) %>% mutate(year=substring(week_ending, 1,4))

top_shows <- grosses_new %>% 
  group_by(show) %>% count(show, wt=real_weekly_gross, sort=T) %>% 
  arrange(desc(n)) %>% filter(n>13300000)

#gross by top show
grosses_new %>%
  filter(year < 2020, show %in% top_shows$show) %>%
  group_by(week_ending, show) %>%
  summarise(real=sum(real_weekly_gross)) %>%
  ggplot(aes(x=week_ending, y=real, group=1)) + geom_point(alpha=0.5) +
  theme_minimal(base_size = 12) + scale_y_continuous(breaks=c("0"=0,"1m"=1000000,"2m"=2000000,"3m"=3000000)) +
  labs(title="Talk less, smile more: Top Broadway show trajectories.", 
       subtitle="Real gross weekly earnings per Broadway show by year. Top 15 grossing shows. Earnings in 2020 dollars", x="", y="") +
  facet_wrap(~show, ncol=5) + geom_smooth(col="darkorange", se=F) +
  theme(plot.title = element_text(size=30, 
                                  vjust=1, family="Impact"))
ggsave("broadway.png", dpi="retina", width=15)
