SELECT user_id 
FROM tokens 
	INNER JOIN (
		SELECT user_id FROM authors WHERE id IN (
			SELECT author FROM news WHERE id = ?
		)
	) t1 USING (user_id)
