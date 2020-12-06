# Libraries ----
library(tidyquant)
library(tidyverse)

# Local Variables ----
symbols       <- c("VTI", "TLT", "IEF", "GLD", "DBC")
investment_rp <- 100000

# DB with Portfolio Data ----
db_portfolio <- symbols %>% 
  tq_get(get  = "stock.prices",
         from = Sys.Date() - lubridate::years(100),
         to   = Sys.Date()) %>% 
  select(symbol, date, adjusted) %>% 
  group_by(symbol) %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly") %>%
  spread(symbol, monthly.returns) %>%
  na.omit() %>%
  pivot_longer(symbols, "Asset") %>%
  tq_portfolio(assets_col   = Asset,
               returns_col  = value,
               weights      = c(0.3, 0.4, 0.15, 0.075, 0.075),
               rebalance_on = "quarter") %>%
  purrr::set_names(c("Date", "Returns")) %>%
  dplyr::mutate(Log_Returns = log(1 + Returns),
                NAV         = investment_rp,
                DD          = 0,
                Year        = year(Date),
                Quarter     = quarter(Date),
                Month       = month(Date)) %>%
  dplyr::select(Date, Month, Quarter, Year, everything()) %>%
  as_tibble()

# Adding NAV ----
for(i in 2:nrow(db_portfolio)){
  db_portfolio$NAV[i] <- db_portfolio$NAV[i - 1]*(1 + db_portfolio$Returns[i])
  db_portfolio$DD[i]  <- db_portfolio$NAV[i]/max(db_portfolio$NAV[1:i]) - 1
}

# Calendar Returns ----
YTD_Returns <- db_portfolio %>%
  dplyr::select(Year, Log_Returns) %>%
  group_by(Year) %>%
  replace(is.na(.), 0) %>%
  summarise_each(funs(sum)) %>%
  dplyr::mutate(YTD = round((exp(Log_Returns) - 1)*100, digits = 2)) %>%
  dplyr::select(-Log_Returns)

Month_Names <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

db_portfolio %>%
  dplyr::select(Year, Month, Log_Returns) %>%
  group_by(Year, Month) %>%
  replace(is.na(.), 0) %>%
  summarise_each(funs(sum)) %>%
  dplyr::mutate(Port_Monthly_Return = exp(Log_Returns) - 1,
                Port_Monthly_Return = round(Port_Monthly_Return*100, digits = 2)) %>%
  dplyr::select(-Log_Returns) %>%
  spread(Month, Port_Monthly_Return) %>%
  left_join(YTD_Returns, by = "Year") %>%
  `colnames<-`(c("Year", Month_Names, "YTD")) %>%
  column_to_rownames(var = "Year") %>%
  replace(is.na(.), "") %>%
  mutate(Jan = cell_spec(Jan, color = ifelse(Jan > 0, "blue", "red"), bold = T),
         Feb = cell_spec(Feb, color = ifelse(Feb > 0, "blue", "red"), bold = T),
         Mar = cell_spec(Mar, color = ifelse(Mar > 0, "blue", "red"), bold = T),
         Apr = cell_spec(Apr, color = ifelse(Apr > 0, "blue", "red"), bold = T),
         May = cell_spec(May, color = ifelse(May > 0, "blue", "red"), bold = T),
         Jun = cell_spec(Jun, color = ifelse(Jun > 0, "blue", "red"), bold = T),
         Jul = cell_spec(Jul, color = ifelse(Jul > 0, "blue", "red"), bold = T),
         Aug = cell_spec(Aug, color = ifelse(Aug > 0, "blue", "red"), bold = T),
         Sep = cell_spec(Sep, color = ifelse(Sep > 0, "blue", "red"), bold = T),
         Oct = cell_spec(Oct, color = ifelse(Oct > 0, "blue", "red"), bold = T),
         Nov = cell_spec(Nov, color = ifelse(Nov > 0, "blue", "red"), bold = T),
         Dec = cell_spec(Dec, color = ifelse(Dec > 0, "blue", "red"), bold = T),
         YTD = cell_spec(YTD, color = ifelse(YTD > 0, "blue", "red"), bold = T),
         Year = YTD_Returns$Year) %>%
  select(Year, everything()) %>%
  knitr::kable(escape = F, align = "c") %>%
  kable_styling("striped", full_width = F)  %>%
  footnote(general = "",
           number = c("Numbers shown are in Percentage"))

# Equity Chart ----
ggplot(db_portfolio, aes(x = Date, y = NAV)) + 
  geom_line(alpha = 0.70, size = 1, colour = "steelblue") +
  theme_tq() +
  labs(title    = str_glue("Equity Curve - Initial NAV: {dollar_format(prefix = '', suffix = '$')(investment_rp)}"),
       subtitle = "Monthly Scale",
       caption  = "By: Carlos Jimenez",
       x        = "",
       y        = "Net Asset Value") +
  scale_y_continuous(labels = scales::dollar)

# Drawdown ----
ggplot(db_portfolio, aes(x = Date, y = DD)) + 
  geom_line(alpha = 0.70, size = 1, colour = "steelblue") +
  theme_tq() +
  labs(title    = "Strategy Drawdown",
       subtitle = "Monthly Scale",
       caption  = "By: Carlos Jimenez",
       x        = "",
       y        = "%") +
  scale_y_continuous(labels = scales::percent)

# Key KPI ----
  # Total MTD 
  Total_MTD <- db_portfolio %>%
    dplyr::filter(Year == year(Sys.Date())) %>%
    dplyr::select(Year, Month, Log_Returns) %>%
    dplyr::group_by(Month) %>% 
    summarise(MTD = sum(Log_Returns), .groups = 'drop') %>%
    tail(n = 1) %>%
    dplyr::mutate(MTD = exp(MTD) - 1) %>%
    dplyr::select(MTD) %>%
    pull(1) %>%
    percent(accuracy = 0.01)
  
  # Total QTD 
  Total_QTD <- db_portfolio %>%
    dplyr::filter(Year == year(Sys.Date())) %>%
    dplyr::select(Year, Quarter, Log_Returns) %>%
    dplyr::group_by(Quarter) %>% 
    summarise(QTD = sum(Log_Returns), .groups = 'drop') %>%
    tail(n = 1) %>%
    dplyr::mutate(QTD = exp(QTD) - 1) %>%
    dplyr::select(QTD) %>%
    pull(1) %>%
    percent(accuracy = 0.01)
  
  # Total QTD 
  Total_YTD <- db_portfolio %>%
    dplyr::filter(Year == year(Sys.Date())) %>%
    dplyr::select(Year, Log_Returns) %>%
    dplyr::group_by(Year) %>% 
    summarise(YTD = sum(Log_Returns), .groups = 'drop') %>%
    tail(n = 1) %>%
    dplyr::mutate(YTD = exp(YTD) - 1) %>%
    dplyr::select(YTD) %>%
    pull(1) %>%
    percent(accuracy = 0.01)
  
  # Total Returns
  Total_Returns <- (db_portfolio %>% dplyr::select(NAV) %>% tail(n = 1) %>% pull(1) / 
                    db_portfolio %>% dplyr::select(NAV) %>% head(n = 1) %>% pull(1) - 1) %>%
    percent(accuracy = 0.01)
  
  # Annualize data (Return - Volatility - CAGR)
  data_annualized <- PerformanceAnalytics::table.AnnualizedReturns(db_portfolio %>% 
                                                                     dplyr::select(Date, Returns) %>%
                                                                     column_to_rownames(var = "Date") %>%
                                                                     as.xts(),
                                                                   scale = 12,
                                                                   Rf    = 0)
  # Max Drawdown
  Max_DD <- db_portfolio %>% dplyr::select(DD) %>% min() %>% percent(accuracy = 0.01)
  
  # Max Return
  Max_Return_Monthly <- db_portfolio %>% dplyr::select(Returns) %>% max() %>% percent(accuracy = 0.01)
  
  # Min Return
  Min_Return_Monthly <- db_portfolio %>% dplyr::select(Returns) %>% min() %>% percent(accuracy = 0.01)
  
  # Positive_Months
  Positive_Months <- db_portfolio %>%
    dplyr::select(Year, Month, Log_Returns) %>%
    group_by(Year, Month) %>%
    replace(is.na(.), 0) %>%
    summarise_each(funs(sum)) %>%
    dplyr::mutate(Port_Monthly_Return = exp(Log_Returns) - 1,
                  Port_Monthly_Return = round(Port_Monthly_Return*100, digits = 2)) %>%
    dplyr::select(-Log_Returns) %>%
    spread(Month, Port_Monthly_Return) %>%
    replace(is.na(.), 0) %>%
    as.data.frame() %>%
    dplyr::select(-Year)
  
  Positive_Months <- (sum(Positive_Months > 0) / sum(Positive_Months > -1000)) %>% percent(accuracy = 0.01)

# Show KPI ----
data.frame(Total_Returns         = Total_Returns,
           CAGR                  = data_annualized$Returns[1] %>% percent(accuracy = 0.01),
           Annualized_Volatility = data_annualized$Returns[2] %>% percent(accuracy = 0.01),
           Annualized_Sharpe     = data_annualized$Returns[3],
           Max_DD                = Max_DD,
           Max_Return_Monthly    = Max_Return_Monthly,
           Min_Return_Monthly    = Min_Return_Monthly,
           Positive_Months       = Positive_Months,
           MTD                   = Total_MTD,
           QTD                   = Total_QTD,
           YTD                   = Total_YTD) %>%
  gather() %>%
  dplyr::mutate(key = c("Total Returns", "CAGR", "Annualized Volatility", "Annualized Sharpe Ratio", "Max Drawdown", "Best Monthly Return", "Worst monthly return", "% Positive Months", "Month-to-Date", "Quarter-to-Date", "Yield-to-Date")) %>%
  purrr::set_names(c("Metric", "Value")) %>%
  kable(escape = F, align = c("l", "c")) %>%
  kable_styling("striped", full_width = F)
