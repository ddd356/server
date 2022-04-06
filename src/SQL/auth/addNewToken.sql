BEGIN;

DELETE FROM public.tokens WHERE user_id IN (SELECT id FROM public.users WHERE login = ?);

INSERT INTO public.tokens
SELECT id, ? FROM public.users WHERE login = ?;
END ;
