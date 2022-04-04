WITH 
usr_data as (
    SELECT
        users.firstname as firstname,
        users.lastname as lastname,
        authors.id as a_id
    FROM authors
        LEFT JOIN users
            ON authors.user_id = users.id),

tkn as (
    SELECT
        authors.id as a_id
    FROM authors
        INNER JOIN tokens
            ON authors.user_id = tokens.user_id
    WHERE
        tokens.token = ?),

news_and_drafts as (
    SELECT * FROM news
	LEFT JOIN usr_data
    ON author = usr_data.a_id),

t1 as (
    SELECT id, short_name, create_date, author, firstname, lastname, category, text, main_picture   FROM news_and_drafts 
	WHERE not draft

    UNION

    SELECT id, short_name, create_date, author, firstname, lastname, category, text, main_picture FROM news_and_drafts
	INNER JOIN tkn
    ON author = tkn.a_id
	WHERE draft AND ?
