create function jsonb_post_pictures (i integer) returns jsonb as '
select
	jsonb_agg(jsonb_build_object(''URI'', ''/pictures?action=get_picture&id='' || np.pictures_id))
from news_pictures np
where
	np.news_id = i' LANGUAGE SQL;
