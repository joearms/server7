%%%=============================================================================
%%% @author Adam Lindberg 
%%%	    Fredrik Svensson
%%% @doc This module contains string metric operations.
%%%
%%% Currently only the function {@link levenshtein/2}.
%%% @end
%%%=============================================================================

-module(string_metrics).

-uuid("84e39f97-9e18-4e32-a588-b4f99e05512c").

-tags([levenshtein,manhatten,distance,keywords]).

-description("distance between two keywords").

-export([levenshtein/2, sim/2]).

sim(X, Y) ->
    1/(1+levenshtein(X, Y)).


%%------------------------------------------------------------------------------
%% @spec levenshtein(StringA, StringB) -> integer()
%%		StringA = StringB = string()
%% @doc Calculates the Levenshtein distance between two strings
%% @end
%%------------------------------------------------------------------------------

levenshtein(Samestring, Samestring) -> 0;
levenshtein(String, []) -> length(String);
levenshtein([], String) -> length(String);
levenshtein(Source, Target) ->
    levenshtein_rec(Source, Target, lists:seq(0, length(Target)), 1).

%% Recurses over every character in the source string and calculates a list of distances
levenshtein_rec([SrcHead|SrcTail], Target, DistList, Step) ->
    levenshtein_rec(SrcTail, Target, 
		    levenshtein_distlist(Target, DistList, SrcHead, [Step], Step), Step + 1);
levenshtein_rec([], _, DistList, _) ->
	lists:last(DistList).

%% Generates a distance list with distance values for every character in the target string
levenshtein_distlist([TargetHead|TargetTail], [DLH|DLT], SourceChar, NewDistList, LastDist) when 
      length(DLT) > 0 ->
    Min = lists:min([LastDist + 1, hd(DLT) + 1, DLH + dif(TargetHead, SourceChar)]),
    levenshtein_distlist(TargetTail, DLT, SourceChar, NewDistList ++ [Min], Min);
levenshtein_distlist([], _, _, NewDistList, _) ->
    NewDistList.

% Calculates the difference between two characters or other values
dif(C, C) -> 0;
dif(_, _) -> 1.
