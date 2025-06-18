%% @doc
%% Ollama Handler Library
%% Generic library to interact with Ollama API in a simple and flexible way.
%% Supports both default configuration and custom configuration per request.
%% Environment variables can be used to override default settings.

-module(ollama_handler).
-author("Steve Roques").

%% Public API
-export([
    % Core API functions
    chat/1, chat/2,
    generate/1, generate/2,
    generate_with_context/2, generate_with_context/3,

    % Configuration functions
    default_config/0,
    get_env_config/0,
    merge_config/2,

    % Utility functions
    print_result/1,
    format_prompt/2
]).

%% Default configuration constants
-define(DEFAULT_ENDPOINT, "http://localhost:11434/api/generate").
-define(DEFAULT_CHAT_ENDPOINT, "http://localhost:11434/api/chat").
-define(DEFAULT_MODEL, <<"llama2">>).
-define(DEFAULT_STREAM, false).
-define(DEFAULT_TEMPERATURE, 0.7).
-define(DEFAULT_MAX_TOKENS, 1000).

%% Types
-type config() :: #{
    endpoint => string(),
    chat_endpoint => string(),
    model => binary(),
    stream => boolean(),
    temperature => float(),
    max_tokens => integer(),
    system_prompt => binary(),
    additional_options => map()
}.

-type ollama_result() :: {ok, binary()} | {error, term()}.
-type message() :: #{role => binary(), content => binary()}.
-type messages() :: [message()].

%% =============================================================================
%% Public API with default configuration
%% =============================================================================

%% @doc
%% Generate text using a simple prompt with default/environment configuration.
-spec generate(string() | binary()) -> ollama_result().
generate(Prompt) ->
    generate(Prompt, get_env_config()).

%% @doc
%% Chat completion using messages format with default/environment configuration.
-spec chat(messages()) -> ollama_result().
chat(Messages) ->
    chat(Messages, get_env_config()).

%% @doc
%% Generate text with additional context using default/environment configuration.
-spec generate_with_context(string() | binary(), string() | binary()) -> ollama_result().
generate_with_context(Context, Prompt) ->
    generate_with_context(Context, Prompt, get_env_config()).

%% =============================================================================
%% Public API with custom configuration
%% =============================================================================

%% @doc
%% Generate text using a simple prompt with custom configuration.
-spec generate(string() | binary(), config()) -> ollama_result().
generate(Prompt, Config) ->
    Endpoint = maps:get(endpoint, Config, ?DEFAULT_ENDPOINT),
    Model = maps:get(model, Config, ?DEFAULT_MODEL),
    Stream = maps:get(stream, Config, ?DEFAULT_STREAM),
    PromptBinary = ensure_binary(Prompt),
    BasePayload = #{
        <<"model">> => Model,
        <<"prompt">> => PromptBinary,
        <<"stream">> => Stream
    },
    Payload = add_optional_params(BasePayload, Config),
    make_ollama_request(Endpoint, Payload).

%% @doc
%% Chat completion using messages format with custom configuration.
-spec chat(messages(), config()) -> ollama_result().
chat(Messages, Config) ->
    ChatEndpoint = maps:get(chat_endpoint, Config, ?DEFAULT_CHAT_ENDPOINT),
    Model = maps:get(model, Config, ?DEFAULT_MODEL),
    Stream = maps:get(stream, Config, ?DEFAULT_STREAM),
    BasePayload = #{
        <<"model">> => Model,
        <<"messages">> => format_messages(Messages),
        <<"stream">> => Stream
    },
    Payload = add_optional_params(BasePayload, Config),
    make_ollama_request(ChatEndpoint, Payload).

%% @doc
%% Generate text with additional context using custom configuration.
-spec generate_with_context(string() | binary(), string() | binary(), config()) -> ollama_result().
generate_with_context(Context, Prompt, Config) ->
    ContextBinary = ensure_binary(Context),
    PromptBinary = ensure_binary(Prompt),
    CombinedPrompt = <<ContextBinary/binary, "\n\n", PromptBinary/binary>>,
    generate(CombinedPrompt, Config).

%% =============================================================================
%% Configuration functions
%% =============================================================================

%% @doc
%% Get default hardcoded configuration.
-spec default_config() -> config().
default_config() ->
    #{
        endpoint => ?DEFAULT_ENDPOINT,
        chat_endpoint => ?DEFAULT_CHAT_ENDPOINT,
        model => ?DEFAULT_MODEL,
        stream => ?DEFAULT_STREAM,
        temperature => ?DEFAULT_TEMPERATURE,
        max_tokens => ?DEFAULT_MAX_TOKENS,
        additional_options => #{}
    }.

%% @doc
%% Get configuration from environment variables with fallback to defaults.
%% Environment variables:
%% - OLLAMA_ENDPOINT: Ollama API endpoint (default: http://localhost:11434/api/generate)
%% - OLLAMA_CHAT_ENDPOINT: Ollama Chat API endpoint (default: http://localhost:11434/api/chat)
%% - OLLAMA_MODEL: Model name to use (default: llama2)
%% - OLLAMA_TEMPERATURE: Temperature for generation (default: 0.7)
%% - OLLAMA_MAX_TOKENS: Maximum tokens to generate (default: 1000)
%% - OLLAMA_STREAM: Whether to stream responses (default: false)
%% - OLLAMA_SYSTEM_PROMPT: System prompt to use
-spec get_env_config() -> config().
get_env_config() ->
    BaseConfig = default_config(),
    EnvConfig = #{
        endpoint => os:getenv("OLLAMA_ENDPOINT", ?DEFAULT_ENDPOINT),
        chat_endpoint => os:getenv("OLLAMA_CHAT_ENDPOINT", ?DEFAULT_CHAT_ENDPOINT),
        model => list_to_binary(os:getenv("OLLAMA_MODEL", binary_to_list(?DEFAULT_MODEL))),
        stream => parse_boolean_env("OLLAMA_STREAM", ?DEFAULT_STREAM),
        temperature => parse_float_env("OLLAMA_TEMPERATURE", ?DEFAULT_TEMPERATURE),
        max_tokens => parse_integer_env("OLLAMA_MAX_TOKENS", ?DEFAULT_MAX_TOKENS)
    },
    SystemPromptConfig = case os:getenv("OLLAMA_SYSTEM_PROMPT") of
        false -> #{};
        SystemPrompt -> #{system_prompt => list_to_binary(SystemPrompt)}
    end,
    maps:merge(maps:merge(BaseConfig, EnvConfig), SystemPromptConfig).

%% @doc
%% Merge two configurations, with the second one taking precedence.
-spec merge_config(config(), config()) -> config().
merge_config(BaseConfig, OverrideConfig) ->
    maps:merge(BaseConfig, OverrideConfig).

%% =============================================================================
%% Utility functions
%% =============================================================================

%% @doc
%% Print the result of an Ollama operation to stdout.
%% Returns 'ok' if successful, 'error' otherwise.
-spec print_result(ollama_result()) -> ok | error.
print_result({ok, Text}) ->
    io:format("~s~n", [Text]),
    ok;
print_result({error, Reason}) ->
    io:format("Error: ~p~n", [Reason]),
    error.

%% @doc
%% Format a prompt template with given arguments.
%% Similar to io_lib:format but returns binary.
-spec format_prompt(string(), list()) -> binary().
format_prompt(Template, Args) ->
    FormattedList = io_lib:format(Template, Args),
    list_to_binary(FormattedList).

%% =============================================================================
%% Private functions
%% =============================================================================

%% Make HTTP POST request to Ollama API.
make_ollama_request(Endpoint, Payload) ->
    application:start(inets),
    JsonPayload = jsx:encode(Payload),
    Headers = [{"Content-Type", "application/json"}],
    case httpc:request(post, {Endpoint, Headers, "application/json", JsonPayload}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            parse_ollama_response(Body);
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, Body}} ->
            {error, {ollama_error, StatusCode, Body}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% Parse JSON response from Ollama API.
parse_ollama_response(Body) ->
    try
        Json = jsx:decode(list_to_binary(Body)),
        case maps:get(<<"response">>, Json, undefined) of
            undefined ->
                case maps:get(<<"message">>, Json, undefined) of
                    undefined ->
                        {error, no_response_field};
                    Message ->
                        case maps:get(<<"content">>, Message, undefined) of
                            undefined -> {error, no_content_field};
                            Content -> {ok, Content}
                        end
                end;
            Response ->
                {ok, Response}
        end
    catch
        _:Error ->
            {error, {json_parse_error, Error}}
    end.

%% Add optional parameters to payload based on config.
add_optional_params(BasePayload, Config) ->
    OptionalParams = [
        {temperature, <<"temperature">>},
        {max_tokens, <<"max_tokens">>},
        {system_prompt, <<"system">>}
    ],
    lists:foldl(fun({ConfigKey, PayloadKey}, Acc) ->
        case maps:get(ConfigKey, Config, undefined) of
            undefined -> Acc;
            Value -> maps:put(PayloadKey, Value, Acc)
        end
    end, BasePayload, OptionalParams).

%% Format messages for chat API.
format_messages(Messages) when is_list(Messages) ->
    [format_message(Msg) || Msg <- Messages];
format_messages(Message) ->
    [format_message(Message)].

%% Format a single message.
format_message(#{role := Role, content := Content}) ->
    #{
        <<"role">> => ensure_binary(Role),
        <<"content">> => ensure_binary(Content)
    };
format_message({Role, Content}) ->
    #{
        <<"role">> => ensure_binary(Role),
        <<"content">> => ensure_binary(Content)
    }.

%% Ensure value is binary.
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8).

%% Parse boolean from environment variable.
parse_boolean_env(EnvVar, Default) ->
    case os:getenv(EnvVar) of
        "true" -> true;
        "1" -> true;
        true -> true;
        _ -> Default
    end.

%% Parse float from environment variable.
parse_float_env(EnvVar, Default) ->
    case os:getenv(EnvVar) of
        false -> Default;
        Value ->
            try
                list_to_float(Value)
            catch
                _:_ -> Default
            end
    end.

%% Parse integer from environment variable.
parse_integer_env(EnvVar, Default) ->
    case os:getenv(EnvVar) of
        false -> Default;
        Value ->
            try
                list_to_integer(Value)
            catch
                _:_ -> Default
            end
    end.

