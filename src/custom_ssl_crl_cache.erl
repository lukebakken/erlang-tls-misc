%----------------------------------------------------------------------
%% Purpose: CRL cache that adds caching for HTTP fetched CRLs
%%----------------------------------------------------------------------

-module(custom_ssl_crl_cache).

-include_lib("public_key/include/public_key.hrl"). 

-behaviour(ssl_crl_cache_api).

-export([lookup/3, select/2, fresh_crl/2]).
-export([clear/0]).

%%====================================================================
%% Cache callback API
%%====================================================================

lookup(#'DistributionPoint'{distributionPoint = {fullName, Names}} = DistributionPoint, Issuer, DbHandle) ->
    case maybe_cached_crls(Names, DbHandle) of
        not_found ->
            ssl_crl_cache_lookup(DistributionPoint, Issuer, DbHandle);
        CRLs ->
            CRLs
    end;
lookup(_, _, _) ->
    not_available.

ssl_crl_cache_lookup(DistributionPoint, Issuer, DbHandle) ->
    case ssl_crl_cache:lookup(DistributionPoint, Issuer, DbHandle) of
        not_available ->
            not_available;
        {_LoggerInfo, CRLs} = RV ->
            maybe_insert(DistributionPoint, CRLs),
            RV;
        %% Note:
        %% works around issue fixed here - https://github.com/erlang/otp/pull/5854
        [CRLs] when is_list(CRLs) ->
            CRLs;
        CRLs when is_list(CRLs) ->
            maybe_insert(DistributionPoint, CRLs),
            CRLs;
        Unexpected ->
            io:format(standard_error, "[ERROR] unexpected result from ssl_crl_cache:lookup/3 ~p~n", [Unexpected]),
            not_available
    end.

select(Issuer, DbHandle) ->
    ssl_crl_cache:select(Issuer, DbHandle).

fresh_crl(DistributionPoint, CRL) ->
    ssl_crl_cache:fresh_crl(DistributionPoint, CRL).

%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------

clear() ->
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

maybe_cached_crls([], _DbHandle) ->
    not_found;
maybe_cached_crls([{uniformResourceIdentifier, "http"++_ = URL} | Rest], DbHandle) ->
    case cache_lookup(URL, DbHandle) of
        [] ->
            maybe_cached_crls(Rest, DbHandle);
        %% Note:
        %% works around issue fixed here - https://github.com/erlang/otp/pull/5854
        [CRLs] when is_list(CRLs) ->
            CRLs;
        CRLs when is_list(CRLs) ->
            CRLs
    end;
maybe_cached_crls([ _| Rest], DbHandle) ->
    %% non-HTTP or HTTPS CRL location
    maybe_cached_crls(Rest, DbHandle).

cache_lookup(_, undefined) ->
    [];
cache_lookup(URL, {{Cache, _}, _}) ->
    #{path :=  Path} = uri_string:normalize(URL, [return_map]),
    case ssl_pkix_db:lookup(string:trim(Path, leading, "/"), Cache) of
        undefined ->
            [];
        %% Note:
        %% works around issue fixed here - https://github.com/erlang/otp/pull/5854
        [CRLs] when is_list(CRLs) ->
            CRLs;
        CRLs when is_list(CRLs) ->
            CRLs
    end.

maybe_insert(#'DistributionPoint'{distributionPoint = {fullName, Names}}, CRLs) ->
    maybe_insert_names(Names, CRLs);
maybe_insert(_, _) ->
    ok.

maybe_insert_names([], _CRLs) ->
    ok;
maybe_insert_names([{uniformResourceIdentifier, "http"++_ = URL} | Rest], CRLs) ->
    % TODO log errors
    % io:format(standard_error, "[DEBUG] insert URL: ~p CRLs ~p~n", [URL, CRLs]),
    ssl_crl_cache:insert(URL, {der, CRLs}),
    maybe_insert_names(Rest, CRLs);
maybe_insert_names([_ | Rest], CRLs) ->
    %% non-HTTP or HTTPS CRL location
    %% TODO log this?
    maybe_insert_names(Rest, CRLs).
