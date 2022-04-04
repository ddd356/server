
    UNION

    SELECT * FROM news 
	LEFT JOIN tkn
    ON author = tkn.a_id
	WHERE draft
