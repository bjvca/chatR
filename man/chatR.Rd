% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chatR.R
\name{chatR}
\alias{chatR}
\title{chatR: A function to interact with OpenAI's ChatGPT API.}
\usage{
chatR(
  question,
  model = "gpt-4-turbo",
  my_API = Sys.getenv("key1"),
  use_history = TRUE
)
}
\arguments{
\item{question}{The question to send to the ChatGPT API.}

\item{model}{The ChatGPT model to use (default: "gpt-4-turbo").}

\item{my_API}{API key taken from global env vars.}

\item{use_history}{Whether to include conversation history (default: TRUE).}
}
\value{
The response from ChatGPT as a string.
}
\description{
chatR: A function to interact with OpenAI's ChatGPT API.
}
