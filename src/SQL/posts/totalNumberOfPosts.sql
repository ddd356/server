SELECT count(*) FROM news 
	LEFT JOIN (
        SELECT
            users.firstname as firstname,
            users.lastname as lastname,
            authors.id as a_id
        FROM authors
            LEFT JOIN users
            ON authors.user_id = users.id) subquery1
    ON author = subquery1.a_id
	WHERE not draft
