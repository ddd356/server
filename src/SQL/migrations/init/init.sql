BEGIN;

CREATE TABLE IF NOT EXISTS public.users
(
    firstname character varying(100),
    lastname character varying(100),
    avatar bytea,
    login character varying(100),
    password character varying(100),
    create_date timestamp(0) without time zone,
    admin boolean,
    id serial NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS public.authors
(
    id serial NOT NULL,
    description text,
    user_id integer NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS public.tags
(
    name character varying(100),
    id serial NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS public.news
(
    short_name character varying(300) NOT NULL,
    create_date timestamp(0) without time zone,
    author integer NOT NULL,
    id serial NOT NULL,
    category integer NOT NULL,
    text text,
    main_picture integer,
    draft boolean NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS public.comments
(
    id integer NOT NULL,
    news_post integer NOT NULL,
    text text,
    PRIMARY KEY (id, news_post)
);

CREATE TABLE IF NOT EXISTS public.categories
(
    id serial NOT NULL,
    name character varying(300) NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS public.news_tags
(
    news_id integer NOT NULL,
    tag_id integer NOT NULL,
    PRIMARY KEY (news_id, tag_id)
);

CREATE TABLE IF NOT EXISTS public.pictures
(
    id serial NOT NULL,
    picture bytea,
    PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS public.news_pictures
(
    news_id integer NOT NULL,
    pictures_id integer NOT NULL,
    PRIMARY KEY (news_id, pictures_id)
);
CREATE TABLE IF NOT EXISTS public.tokens
(
    user_id integer NOT NULL,
    token character(20) NOT NULL,
    PRIMARY KEY (user_id)
);
CREATE TABLE IF NOT EXISTS public.version
(
    version integer
);

ALTER TABLE IF EXISTS public.authors
    ADD FOREIGN KEY (user_id)
    REFERENCES public.users (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;


ALTER TABLE IF EXISTS public.news
    ADD FOREIGN KEY (author)
    REFERENCES public.authors (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;


ALTER TABLE IF EXISTS public.news
    ADD FOREIGN KEY (category)
    REFERENCES public.categories (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;


ALTER TABLE IF EXISTS public.news
    ADD FOREIGN KEY (main_picture)
    REFERENCES public.pictures (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;


ALTER TABLE IF EXISTS public.comments
    ADD FOREIGN KEY (news_post)
    REFERENCES public.news (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;


ALTER TABLE IF EXISTS public.news_tags
    ADD FOREIGN KEY (news_id)
    REFERENCES public.news (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;


ALTER TABLE IF EXISTS public.news_tags
    ADD FOREIGN KEY (tag_id)
    REFERENCES public.tags (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;


ALTER TABLE IF EXISTS public.news_pictures
    ADD FOREIGN KEY (news_id)
    REFERENCES public.news (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;


ALTER TABLE IF EXISTS public.news_pictures
    ADD FOREIGN KEY (pictures_id)
    REFERENCES public.pictures (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;

ALTER TABLE IF EXISTS public.tokens
    ADD FOREIGN KEY (user_id)
    REFERENCES public.users (id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION
    NOT VALID;

ALTER TABLE IF EXISTS public.users
    ADD CONSTRAINT unique_login UNIQUE (login);

INSERT INTO public.users (firstname, lastname, avatar, login, password, create_date, admin, can_create_posts) VALUES ('admin', 'admin', '' , 'admin', ?, ?, true, true);

INSERT INTO public.version (version) VALUES (1);
END;
