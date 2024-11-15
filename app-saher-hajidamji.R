# ===============================================
# Fill in the following fields
# ===============================================
# Title: Harry Potter Sentiment and Word Analysis
# Description: Trends on the words of each of the Harry Potter book series
# Details: Play around with the toggles!
# Author: Saher Hajidamji
# Date: Saturday Nov 9, 2024


# ===============================================
# Required packages
# (you can use other packages if you want to)
# ===============================================
# ===============================================
# Required packages
# ===============================================
library(shiny)
library(tidyverse)
library(tidytext)
library(DT)
library(plotly)
library(wordcloud)

# Import data
tbl <- read_csv(file = "harry_potter_books.csv.crdownload", col_types = "ccc")

# Tokenize text
tbl_tokens <- tbl |> 
  unnest_tokens(word, text)

bing <- read_csv("bing.csv")
afinn <- read_csv("afinn.csv")
nrc <- read_csv("nrc.csv")
loughran <- read_csv("loughran.csv")

ui <- fluidPage(
  
  tags$style(HTML("
    body {
      background-color: pink;
    }
  ")),
  
  titlePanel("Harry Potter Sentiment and Word Analysis"),
  
  # Arrange input widgets in a single row with four columns
  fluidRow(
    column(3, 
           radioButtons(
             inputId = "lexicon_dropdown",            
             label = "Select a Sentiment Lexicon:",   
             choices = c("Bing", "Nrc", "Loughran"),
             selected = "Bing"
           )
    ),
    column(3,
           sliderInput(
             inputId = "numofwords_slider",           
             label = "Num of Words in Each Section:",    
             min = 1,                     
             max = 300,                 
             value = 300        
           )
    ),
    column(3,
           numericInput(
             inputId = "wordcloudslider",           
             label = "Num of Words in Word Cloud:",    
             min = 1,                     
             max = 300,                 
             value = 300
           )
    ),
    column(3,
           selectInput(
             inputId = "bookdropdown",            
             label = "Choose a Book:",   
             choices = c("Book 1: Philosopher's Stone", "Book 2: Chamber of Secrets", 
                         "Book 3: Prisoner of Azkaban", "Book 4: Goblet of Fire",
                         "Book 5: Order of the Phoenix", "Book 6: Half Blood Prince",
                         "Book 7: Deathly Hallows"),
             selected = "Book 1: Philosopher's Stone"
           )
    )
  ),
  
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Sentiment Analysis by Section",
                       h3("Sentiment Analysis"),
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel(title = "Word Frequency Analysis", 
                       h3("Word Analysis"),
                       plotOutput("plot2", height = 500),
                       hr(),
                       dataTableOutput('table2')
              )
  )
  
)


server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    diff <- input$numofwords_slider
    num_total_words <- nrow(tbl_tokens)
    
    lexicon <- tolower(input$lexicon_dropdown)
    if (lexicon == "bing") {
      sentiment_lexicon = bing
    } else if (lexicon == "loughran") {
      sentiment_lexicon = loughran
    } else {
      sentiment_lexicon = nrc
    }
    
    # Calculate cumulative book row counts for transitions
    book_counts <- tbl_tokens %>%
      group_by(book) %>%
      summarise(count = n()) %>%
      mutate(cumulative_count = cumsum(count)) %>%
      mutate(section_transition = cumulative_count / diff) # Convert to section scale
    
    book_transition_lines <- book_counts$section_transition
    
    sentiment_scores_by_book_section <- tbl_tokens %>%
      left_join(sentiment_lexicon, by = "word") %>%
      mutate(sentiment = case_when(
        sentiment == "positive" ~ 1,
        sentiment == "negative" ~ -1,
        TRUE ~ 0
      )) %>% 
      mutate(linenumber = row_number()) %>% 
      mutate(section = (linenumber %/% diff) + 1) %>% 
      group_by(book, section) %>% 
      summarize(score = sum(sentiment))
    
    sentiment_scores_by_book_section %>% 
      ggplot(aes(x = section, y = score)) +
      geom_col(aes(fill = book))
  })
  
  output$table1 <- renderDataTable({
    
    lexicon <- tolower(input$lexicon_dropdown)
    if (lexicon == "bing") {
      sentiment_lexicon <- bing
    } else if (lexicon == "loughran") {
      sentiment_lexicon <- loughran
    } else {
      sentiment_lexicon <- nrc
    }
    
    word_scores <- tbl_tokens %>%
      left_join(sentiment_lexicon, by = "word") %>%
      mutate(sentiment = case_when(
        sentiment == "positive" ~ 1,
        sentiment == "negative" ~ -1,
        TRUE ~ 0
      )) %>%
      group_by(word) %>%
      summarise(score = sum(sentiment)) %>%
      arrange(desc(score))
    
    # Display only `word` and `score` columns
    word_scores %>%
      select(word, score)
    
  })
  
  output$plot2 <- renderPlot({
    
    tbl_tokens %>%
      filter(book == input$bookdropdown) %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = input$wordcloudslider, colors = rainbow(7)))
    
  })
  
  output$table2 <- renderDataTable({
    tbl_tokens %>%
      filter(book == input$bookdropdown) %>%
      anti_join(stop_words) %>%
      count(word) %>%
      arrange(desc(n)) %>%
      head(n=25)
  })
  
}

shinyApp(ui = ui, server = server)


