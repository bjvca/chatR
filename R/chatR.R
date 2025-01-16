
#' chatR: A function to interact with OpenAI's ChatGPT API.
#'
#' @param question The question to send to the ChatGPT API.
#' @param model The ChatGPT model to use (default: "gpt-4-turbo").
#' @param my_API API key taken from global env vars.
#' @param use_history Whether to include conversation history (default: TRUE).
#' @return The response from ChatGPT as a string.
#' @export






chatR <- function(question, model = "gpt-4-turbo",my_API = Sys.getenv("key1"), use_history = TRUE) {

  # Loading Required Libraries
  library(httr)
  library(jsonlite)
  library(stringr)

  # Define the API endpoint
  url <- "https://api.openai.com/v1/chat/completions"

  # Static environment to track history across calls
  if (!exists(".chatR_history", envir = .GlobalEnv)) {
    assign(".chatR_history", list(), envir = .GlobalEnv)
  }

  # Always update the global history
  global_history <- get(".chatR_history", envir = .GlobalEnv)
  global_history <- c(global_history, list(list(role = "user", content = question)))

  # Create the request body based on the use_history parameter
  body <- list(
    model = model,
    messages = c(
      list(list(role = "system", content = "You are a helpful assistant.")),
      if (use_history) global_history else list(list(role = "user", content = question))
    )
  )

  # Serialize the body to JSON using jsonlite
  json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)

  # Make the API call
  response <- POST(
    url,
    add_headers(
      Authorization = paste("Bearer", my_API),
      `Content-Type` = "application/json"
    ),
    body = json_body,
    encode = "raw" # Use raw encoding for properly formatted JSON
  )

  # Check for errors in the response
  if (http_status(response)$category != "Success") {
    stop("Error: ", http_status(response)$reason, " - ", content(response, "text", encoding = "UTF-8"))
  }

  # Parse the response
  content <- fromJSON(content(response, "text", encoding = "UTF-8"))

  # Extract the assistant's response
  assistant_response <- content$choices$message$content


  # Append the assistant's response to the global history
  global_history <- c(global_history, list(list(role = "assistant", content = assistant_response)))

  # Update the global history
  assign(".chatR_history", global_history, envir = .GlobalEnv)

  # Return only the assistant's response
  return(cat(assistant_response))
}

# Example usage
# Reset history: assign(".chatR_history", list(), envir = .GlobalEnv)
# response <- chatR("What is the capital of France?")
# print(response)
# response <- chatR("What is my name?", use_history = TRUE)
# print(response)



