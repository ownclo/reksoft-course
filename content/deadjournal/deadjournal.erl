-module(deadjournal).
-export([start/0,
         stop/0,
         new_post/2,
         get_posts/0,
         get_post/1,
         delete_post/1,
         new_comment/2,
         get_comments/1
        ]).

% Storing comments together with post can cause
% serious performance problems, but for now it
% just makes everything much simpler. We avoid
% the need of cascading delete and of generating
% uniquie post ids.
-record(post, {
        name :: atom(),
        % use binary for performance if needed
        text :: string(),
        comments = [] :: [string()]
        }).

start() ->
    mnesia:start(),
    mnesia:create_table(post, [
            {attributes, record_info(fields, post)}]),
    ok.

stop() ->
    mnesia:stop().

%% XXX: Will silently owerride old post in case
%% of matching names.
new_post(Name, Text) ->
    mnesia:transaction(fun() ->
        mnesia:write(#post{name = Name, text = Text})
        end),
    ok.

get_posts() ->
    {atomic, Keys} = mnesia:transaction(fun() ->
        mnesia:all_keys(post) end),
    Keys.

get_post(Name) ->
    {atomic, Post} = mnesia:transaction(fun() ->
        read_post(Name) end),

    case Post of
        none -> none;
        Post -> Post#post.text
    end.

new_comment(Name, Text) ->
    mnesia:transaction(fun() ->
        Post = read_post(Name),
        Comments = Post#post.comments,
        NewPost = Post#post{ comments = [Text | Comments] },
        mnesia:write(NewPost)
    end),
    ok.

get_comments(PostName) ->
    {atomic, Comments} = mnesia:transaction(fun() ->
        Post = read_post(PostName),
        Post#post.comments
    end),
    Comments.

delete_post(PostName) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:delete({post, PostName})
    end),
    ok.

%% Interanal functions.

read_post(Name) ->
    Res = mnesia:read(post, Name),
    case Res of
        [] -> none;
        [Post] -> Post
    end.
