create function jsonb_post_tags (i integer)returns jsonb as '
select
	jsonb_agg(jsonb_build_object(''name'', t.name, ''id'', t.id))
from news_tags nt
	left join tags as t
		on t.id = nt.tag_id
where
	nt.news_id = i' LANGUAGE SQL;
