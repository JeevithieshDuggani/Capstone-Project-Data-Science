suppressWarnings(library(tm))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))

unigram  <- readRDS('unigram.rds')
bigram   <- readRDS('bigram.rds')
trigram  <- readRDS('trigram.rds')
quadgram <- readRDS('quadgram.rds')

cleanInput <- function(inp) {
    inp <- gsub('#\\S+', '', inp)            ## Remove Hashtags
    inp <- gsub('@\\S+', '', inp)            ## Remove Mentions
    inp <- gsub('[[:cntrl:]]', '', inp)      ## Remove Controls and special characters
    inp <- gsub("\\d", '', inp)              ## Remove Controls and special characters
    inp <- gsub('[[:punct:]]', '', inp)      ## Remove Punctuations
    inp <- gsub("-", "", inp)                ## remove hypens
    inp <- gsub("^[[:space:]]*","",inp)      ## Remove leading whitespaces
    inp <- gsub("[[:space:]]*$","",inp)      ## Remove trailing whitespaces
    inp <- gsub(' +',' ',inp)                ## Remove einptra whitespaces
    
    inp <- removeNumbers(removePunctuation(tolower(inp)))
    ans <- strsplit(inp, " ")[[1]]
    
    return(ans)
}

Predict <- function(inp) {
    
    ans <- cleanInput(inp)
    if (length(ans) >= 3) {
        ans <- tail(ans, 3)
        chosen <- quadgram[quadgram$word1 == ans[1] & quadgram$word2 == ans[2] & quadgram$word3 == ans[3], 'word4']
        
        if (identical(character(0), head(chosen, 1))){
            return(Predict(paste(ans[2], ans[3], sep = ' ')))
        }
        else {
            return(head(chosen, 1))
        }
    }
    
    else if (length(ans) == 2) {
        chosen <- trigram[trigram$word1 == ans[1] & trigram$word2 == ans[2], 'word3']
        if (identical(character(0), head(chosen, 1))) {
            return(Predict(ans[2]))
        }
        else {
            return(head(chosen, 1))
        }
    }
    else if (length(ans) == 1){
        ans <- tail(ans,1)
        chosen <- bigram[bigram$word1 == ans[1], 'word2']
        
        if (identical(character(0), head(chosen, 1))) {
            return(head(unigram$word, 1))
        }
        else {
            return(head(chosen, 1))
        }
    }
}


shinyServer(
    function(input,output) {
        output$oText <- renderPrint({as.character(input$inputText)})
        dataInput    <- reactive({
            if(input$goButton){Predict(input$inputText)}
        })
        output$nextWord  <- renderPrint({dataInput()})
    }
)