# -- Interactive version of plots
# -- So far, ggplotly() is trash

library(plotly)

my_plot <- ggplot() +
  
  geom_vline(xintercept = 0, size = 0.75,linetype='dashed') +
  geom_hline(yintercept = 0, size = 0.75, linetype='dashed') +
  
  geom_point(data = atp_aces_10_19_df,
             aes(x = ace_rate_above_expected,
                 y = opp_ace_rate_above_expected, 
                 text = player,
                 fill = player_height_cm),
             alpha = 0.75,
             shape = 21,
             size = 2.5
  ) +

  scale_fill_gradient2(midpoint = mean(atp_aces_10_19_df$player_height_cm),
                     low = "#83D0E9", mid = "white",
                     high = "indianred", space = "Lab" ) +
  labs(title = 'Trading Aces: Comparing Aces Hit to Aces Prevented',
       x='Aces Hit  Above Average',
       y = 'Aces Allowed Above Average',
       fill = "Player Height \n(cm)",
       caption="Stats On-The-T\nData: Tennis Abstract &\natptour.com"
  ) + 
  
  scale_x_continuous(
    limits = c(-15,19),
    breaks = c(-10, -5, 0, 5, 10, 15),
    labels=c("-0.1" = "-10 %", '-0.05' ="-5 %", '0' = '0 %', "0.05" = "+5 %", "0.1" = "+10 %",
             "0.15" = "+15 %")
  ) + 
  scale_y_continuous(
    limits = c(-5.5,8.5),
    breaks = c(-5, -2.5, 0, 2.5, 5, 7.5),
    labels=c('-0.05' ="-5 %", "-0.025" = "-2.5 %","0" = "0 %", "0.025" = "+2.5 %",
             '0.05' ="+5 %", '0.075' ="+7.5 %")
  ) +
  plot_theme() +
  theme(legend.position = c(0.05, -0.15))



fig <- ggplotly(my_plot, tooltip="player")
fig

#################################################################
# Seems like more customizable options with plot_ly()
colorScale <- data.frame(z=c(0,0.5,1),col=c("blue","white","red"))
colorScale$col <- as.character(colorScale$col)

fig2 <- plot_ly(data = atp_aces_10_19_df,
                x = ~ace_rate_above_expected, 
                y = ~opp_ace_rate_above_expected,
                text = ~player,
                type = 'scatter',
                mode = 'markers',
                name = '',
                marker = list(size = 8,
                              #reversescale = TRUE,
                              color = ~ player_height_cm,
                              line = list(color = 'black',
                                          width = 1),
                              colorscale = list(c(0, "#83D0E9"), c(0.5, 'white'), c(1, "indianred")),
                              colorbar = list(title='Player Height\n(cm)')),
                hovertemplate = paste('<b>Player: %{text} </b>',
                                      '<br><b>Aces Hit Above Average</b>: %{x:.2f}',
                                      '<br><b>Aces allowed Above Average</b>: %{y:.2f}<br>'
                                      )
                
                )
fig2



fig2 <- fig2 %>% layout(title = 'Trading Aces: Comparing Aces Hit to Aces Prevented',
                      xaxis = list(title = 'Aces Hit Above Average (%)',
                                   range = c(-10, 19)),
                      yaxis = list(title = 'Aces Allowed Above Average (%)',
                                   range = c(-5.5,8.5)))

fig2

Sys.setenv("plotly_username"="petertea96")
Sys.setenv("plotly_api_key"="nnbGNv4ddP2jP6sSfz5F")
api_create(fig2, filename = "Trading_Aces")
