# Ollama Handler

[![Hex.pm](https://img.shields.io/hexpm/v/ollama_handler.svg)](https://hex.pm/packages/ollama_handler)
[![Hex Docs](https://img.shields.io/badge/hex-docs-blue.svg)](https://hexdocs.pm/ollama_handler)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)

A simple and flexible Erlang library for interacting with the Ollama API. This library provides a clean interface to generate text, perform chat completions, and manage configurations for Ollama models.

## Features

- 🚀 **Simple API** - Easy-to-use functions for text generation and chat
- ⚙️ **Flexible Configuration** - Support for default, environment, and custom configurations
- 🔄 **Multiple Endpoints** - Support for both generate and chat APIs
- 🛡️ **Error Handling** - Comprehensive error handling and type safety
- 📝 **Well Documented** - Complete type specifications and documentation
- 🔧 **Environment Variables** - Easy configuration through environment variables

## Prerequisites

- Erlang/OTP 24 or higher
- [Ollama](https://ollama.ai/) running locally or remotely
- A downloaded model (e.g., `ollama pull llama2`)

## Quick Start

### Basic Text Generation

```erlang
% Start your Erlang shell
1> application:start(inets).
ok
2> {ok, Response} = ollama_handler:generate("Explain quantum computing in simple terms").
{ok, <<"Quantum computing is a revolutionary computing paradigm...">>}
3> ollama_handler:print_result({ok, Response}).
Quantum computing is a revolutionary computing paradigm...
ok
```

### Chat Completion

```erlang
1> Messages = [
    #{role => <<"user">>, content => <<"Hello! How are you today?">>}
].
2> {ok, Response} = ollama_handler:chat(Messages).
{ok, <<"Hello! I'm doing well, thank you for asking...">>}
```

### With Custom Configuration

```erlang
1> Config = #{
    model => <<"codellama">>,
    temperature => 0.3,
    max_tokens => 500
}.
2> {ok, Response} = ollama_handler:generate("Write a Python function to sort a list", Config).
```

## API Reference

### Core Functions

#### `generate/1,2`
Generate text from a simple prompt.

```erlang
-spec generate(string() | binary()) -> ollama_result().
-spec generate(string() | binary(), config()) -> ollama_result().
```

**Examples:**
```erlang
ollama_handler:generate("What is the meaning of life?").
ollama_handler:generate("Explain AI", #{model => <<"phi3">>, temperature => 0.8}).
```

#### `chat/1,2`
Perform chat completion using the messages format.

```erlang
-spec chat(messages()) -> ollama_result().
-spec chat(messages(), config()) -> ollama_result().
```

**Examples:**
```erlang
Messages = [
    #{role => <<"system">>, content => <<"You are a helpful assistant">>},
    #{role => <<"user">>, content => <<"Hello!">>}
],
ollama_handler:chat(Messages).
```

#### `generate_with_context/2,3`
Generate text with additional context.

```erlang
-spec generate_with_context(string() | binary(), string() | binary()) -> ollama_result().
-spec generate_with_context(string() | binary(), string() | binary(), config()) -> ollama_result().
```

**Examples:**
```erlang
Context = "You are a expert in mathematics",
Prompt = "Explain calculus",
ollama_handler:generate_with_context(Context, Prompt).
```

### Configuration Functions

#### `default_config/0`
Get the default hardcoded configuration.

```erlang
Config = ollama_handler:default_config().
```

#### `get_env_config/0`
Get configuration from environment variables with fallback to defaults.

```erlang
Config = ollama_handler:get_env_config().
```

#### `merge_config/2`
Merge two configurations, with the second taking precedence.

```erlang
BaseConfig = ollama_handler:default_config(),
CustomConfig = #{model => <<"llama2">>, temperature => 0.9},
FinalConfig = ollama_handler:merge_config(BaseConfig, CustomConfig).
```

### Utility Functions

#### `print_result/1`
Print the result of an operation to stdout.

```erlang
Result = ollama_handler:generate("Hello world"),
ollama_handler:print_result(Result).
```

#### `format_prompt/2`
Format a prompt template with arguments.

```erlang
Prompt = ollama_handler:format_prompt("Translate '~s' to ~s", ["hello", "French"]).
```

## Configuration

### Environment Variables

You can configure the library using environment variables:

| Variable | Description | Default |
|----------|-------------|---------|
| `OLLAMA_ENDPOINT` | Ollama API endpoint for generation | `http://localhost:11434/api/generate` |
| `OLLAMA_CHAT_ENDPOINT` | Ollama API endpoint for chat | `http://localhost:11434/api/chat` |
| `OLLAMA_MODEL` | Default model to use | `llama2` |
| `OLLAMA_TEMPERATURE` | Generation temperature (0.0-1.0) | `0.7` |
| `OLLAMA_MAX_TOKENS` | Maximum tokens to generate | `1000` |
| `OLLAMA_STREAM` | Enable streaming responses | `false` |
| `OLLAMA_SYSTEM_PROMPT` | Default system prompt | (none) |

**Example:**
```bash
export OLLAMA_MODEL="codellama"
export OLLAMA_TEMPERATURE="0.3"
export OLLAMA_ENDPOINT="http://my-ollama-server:11434/api/generate"
```

### Configuration Map

You can also pass configuration directly as a map:

```erlang
Config = #{
    endpoint => "http://localhost:11434/api/generate",
    chat_endpoint => "http://localhost:11434/api/chat",
    model => <<"phi3">>,
    stream => false,
    temperature => 0.8,
    max_tokens => 1500,
    system_prompt => <<"You are a helpful assistant">>,
    additional_options => #{}
}.
```

## Message Format

For chat completions, use the following message format:

```erlang
Messages = [
    #{role => <<"system">>, content => <<"You are a helpful assistant">>},
    #{role => <<"user">>, content => <<"Hello, how can you help me?">>},
    #{role => <<"assistant">>, content => <<"I can help you with various tasks...">>},
    #{role => <<"user">>, content => <<"Tell me about Erlang">>}
].
```

Supported roles:
- `system` - System instructions
- `user` - User messages
- `assistant` - Assistant responses

## Error Handling

The library returns tuples in the format `{ok, Result}` or `{error, Reason}`:

```erlang
case ollama_handler:generate("Hello") of
    {ok, Response} ->
        io:format("Success: ~s~n", [Response]);
    {error, {ollama_error, StatusCode, Body}} ->
        io:format("Ollama API error ~p: ~s~n", [StatusCode, Body]);
    {error, {request_failed, Reason}} ->
        io:format("Request failed: ~p~n", [Reason]);
    {error, {json_parse_error, Error}} ->
        io:format("JSON parsing error: ~p~n", [Error])
end.
```

## Examples

### Building a Simple Chatbot

```erlang
-module(simple_chatbot).
-export([start/0, chat_loop/1]).

start() ->
    application:start(inets),
    InitialMessages = [
        #{role => <<"system">>, content => <<"You are a friendly chatbot">>}
    ],
    chat_loop(InitialMessages).

chat_loop(Messages) ->
    io:format("You: "),
    case io:get_line("") of
        eof -> ok;
        Line ->
            UserMessage = #{role => <<"user">>, content => list_to_binary(string:trim(Line))},
            NewMessages = Messages ++ [UserMessage],
            
            case ollama_handler:chat(NewMessages) of
                {ok, Response} ->
                    io:format("Bot: ~s~n", [Response]),
                    AssistantMessage = #{role => <<"assistant">>, content => Response},
                    chat_loop(NewMessages ++ [AssistantMessage]);
                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason]),
                    chat_loop(Messages)
            end
    end.
```

### Text Summarization

```erlang
-module(text_summarizer).
-export([summarize/1]).

summarize(Text) ->
    Context = "You are an expert at summarizing text. Provide a concise summary.",
    Prompt = "Summarize the following text:\n\n" ++ Text,
    
    Config = #{
        model => <<"llama2">>,
        temperature => 0.3,
        max_tokens => 200
    },
    
    ollama_handler:generate_with_context(Context, Prompt, Config).
```

### Code Generation

```erlang
-module(code_generator).
-export([generate_function/2]).

generate_function(Language, Description) ->
    Prompt = io_lib:format("Write a ~s function that ~s. Include comments and proper formatting.", 
                          [Language, Description]),
    
    Config = #{
        model => <<"codellama">>,
        temperature => 0.2,
        max_tokens => 500
    },
    
    ollama_handler:generate(Prompt, Config).
```

## Development

### Building

```bash
rebar3 compile
```

### Running Tests

```bash
rebar3 eunit
```

### Type Checking

```bash
rebar3 dialyzer
```

### Code Analysis

```bash
rebar3 xref
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [Ollama](https://ollama.ai/) for providing the excellent local LLM platform
- The Erlang/OTP team for the robust runtime system
- [jsx](https://github.com/talentdeficit/jsx) for JSON encoding/decoding



