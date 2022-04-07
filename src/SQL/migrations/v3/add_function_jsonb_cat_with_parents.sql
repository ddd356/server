create function jsonb_cat_with_parents(i integer) returns jsonb as '
with recursive
t1 as (
	select id, name, parent_id as pid, 1 as n
	from categories
	where id = i
	
	union all
	
	select c.id, c.name, c.parent_id as pid, t1.n + 1
	from t1
		join categories c
			on t1.pid = c.id
),

t2 as (
	select jsonb_build_object(''name'', name, ''id'', id, ''parent'', '''') as obj, id, n
	from 
		(select * from t1 order by n desc limit 1) as c
	
	union all
	
	select jsonb_build_object(''name'', c1.name, ''id'', c1.id, ''parent'', c2.obj) as obj,  c1.id, c1.n
	from (select * from t1 
		  order by n desc ) as c1 
		inner join t2 c2 on c1.pid = c2.id
)

select obj as o from t2
order by 1 desc
limit 1' LANGUAGE SQL;
