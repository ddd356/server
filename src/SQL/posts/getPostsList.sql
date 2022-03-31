SELECT
	COALESCE (jsonb_agg(jsonb_build_object(
		'id', id, 
		'short_name', short_name,
		'create_date', create_date,
		'author', author,
		'category', jsonb_cat_with_parents(category),
		'text', text,
		'main_picture', uri_picture(main_picture),
		'tags', jsonb_post_tags(id),
		'pictures', jsonb_post_pictures(id)
	)), '[]')
FROM news
WHERE not draft
