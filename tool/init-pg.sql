begin;

create role web_anon nologin;

grant usage on schema public to web_anon;
grant select on table public.edge, public.generation, public.genome, public.node, public.run to web_anon;

create role authenticator noinherit login password 'aleafhasmanycells';
grant web_anon to authenticator;

commit;
