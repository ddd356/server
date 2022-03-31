create function uri_picture (i integer) returns text as '
select
	''/pictures?action=get_picture&id='' || i' LANGUAGE SQL;
