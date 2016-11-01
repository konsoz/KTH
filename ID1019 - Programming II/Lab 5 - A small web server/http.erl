%% @author konstantin
%% @doc @todo Add description to http.


-module(http).
-compile(export_all).

parse_request(R0) ->
	{Request, R1} = request_line(R0),
	{Headers, R2} = headers(R1),
	{Body, _} = message_body(R2),
	{Request, Headers, Body}.

request_line([$G, $E, $T, 32 |R0]) ->
	{URI, R1} = request_uri(R0),
	{Ver, R2} = http_version(R1),
	[13,10|R3] = R2,
	{{get, URI, Ver}, R3}.

%% request_uri([32|R0])-> {[], R0};
%% request_uri([C|R0]) ->
%% 	{Rest, R1} = request_uri(R0),
%% 	{[C|Rest], R1}.

request_uri(R0) -> request_uri(R0,[]).
request_uri([32|R0], Acc)-> {lists:reverse(Acc), R0};
request_uri([C|R0], Acc) -> request_uri(R0,[C|Acc]).


http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) -> {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) -> {v10, R0}.

headers([13,10|R0]) -> {[],R0};
headers(R0) ->
	{Header, R1} = header(R0),
	{Rest, R2} = headers(R1),
	{[Header|Rest], R2}.
header(R0) -> header(R0,[]).
header([13,10|R0],Acc) -> {lists:reverse(Acc), R0};
header([C|R0],Acc) -> header(R0,[C|Acc]).

message_body(R) -> {R, []}.

ok(Body) -> 
	Chars = to_list(Body),
	"HTTP/1.1 200 OK\r\n" ++ "Content-Lenght:" ++ integer_to_list(length(Chars)) ++ "\r\n" ++ "\r\n" ++ Chars.

to_list(Body) when is_binary(Body) -> binary_to_list(Body);
to_list(Body) -> Body.