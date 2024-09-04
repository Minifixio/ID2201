-module(http).
-export([parse_request/1]).

%% Main function to parse an HTTP request
parse_request(R0) ->
    {RequestLine, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body, _} = message_body(R2),
    {RequestLine, Headers, Body}.

request_line(R0) ->
    SplitResult = re:split(R0, "(?>\r\n|\n|\x0b|\f|\r|\x85)", [{return, list}, {parts, 2}]),
    case SplitResult of
        [RequestLine, R1] -> {RequestLine, R1};
        _ -> {error, "Unexpected split result"}
    end.

headers(R1) ->
    SplitResult = re:split(R1, "(?>\r\n|\n|\x0b|\f|\r|\x85)", [{return, list}, {parts, 2}]),
    case SplitResult of
        [Headers, R2] -> {Headers, R2};
        _ -> {error, "Unexpected split result"}
    end.

message_body(R2) ->
    SplitResult = re:split(R2, "(?>\r\n|\n|\x0b|\f|\r|\x85)", [{return, list}, {parts, 2}]),
    case SplitResult of
        [[], Body] -> {Body, []};
        _ -> {error, "Unexpected split result"}
    end.


% GET /index.html HTTP/1.1\r\nHost: www.example.com\r\nUser-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8\r\nAccept-Language: en-US,en;q=0.5\r\nAccept-Encoding: gzip, deflate\r\nConnection: keep-alive\r\n\r\nusername=johndoe&password=1234
% GET /index.html HTTP/1.1\r\nfoo 34\r\n\r\nHello