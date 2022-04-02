
	ORDER BY ? LIMIT ? OFFSET ? 
)

SELECT
        COALESCE (jsonb_agg(jsonb_build_object(
                'id', t1.id,
                'short_name', short_name,
                'create_date', create_date,
                'author', jsonb_build_object('a_id', author, 'firstname', t1.firstname, 'lastname', t1.lastname),
                'category', jsonb_cat_with_parents(category),
                'text', text,
                'main_picture', uri_picture(main_picture),
                'tags', jsonb_post_tags(t1.id),
                'pictures', jsonb_post_pictures(t1.id)
        )), '[]')
FROM t1
