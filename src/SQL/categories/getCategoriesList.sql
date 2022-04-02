with t1 as (
SELECT id FROM categories ORDER BY id LIMIT ? OFFSET ?)

SELECT 
	COALESCE (jsonb_agg(jsonb_cat_with_parents(id)), '[]')
FROM t1
