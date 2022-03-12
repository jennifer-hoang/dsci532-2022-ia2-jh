library(dash)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")


app$layout(
  dbcContainer(
    list(
      htmlH1("Spotify Explorer"),
      htmlH3("Top Artists by Genre"),
      dccGraph(id = "top_artists"),
      htmlLabel("Artist Genre:"),
      dccDropdown(
        id = "genre_select",
        options = list(
          list(label = "EDM", value = "edm"),
          list(label = "Latin", value = "latin"),
          list(label = "Pop", value = "pop"),
          list(label = "R&B", value = "r&b"),
          list(label = "Rap", value = "rap"),
          list(label = "Rock", value = "rock")
        ),
        value = "pop"
      )
    )
  )
)


app$callback(
  output("top_artists", "figure"),
  list(input("genre_select", "value")),
  function(genre_select) {
    top10_df <- df %>%
      dplyr::filter(playlist_genre == genre_select) %>%
      group_by(track_artist) %>%
      summarise(mean_popularity = mean(track_popularity)) %>%
      arrange(desc(mean_popularity)) %>%
      head(10)

    p <- ggplot(
      top10_df,
      aes(x = mean_popularity, y = reorder(track_artist, mean_popularity))
    ) +
      geom_bar(stat = "identity") +
      labs(x = "Average Track Popularity", y = "Artist")
    ggplotly(p)
  }
)


app$run_server(host= '0.0.0.0')
