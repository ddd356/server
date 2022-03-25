with recursive lvls as (
	select id, name, pid, 0 as lvl
		from categories
	where pid is null
	
union all
	
	select c.id, c.name, c.pid, lvl+1
		from lvls l
			join categories c
				on c.pid = l.id
),

level_? as (
	select pid, jsonb_agg(jsonb_build_object('id', id, 'name', name, 'pid', pid, 'subcat', null)) as js
		from lvls
	where lvl = ?
	group by pid
),
