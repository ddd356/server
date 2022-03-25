with recursive 
par as (
	select pid from categories where not pid is null group by pid
),
solo as (
	select
		jsonb_build_object('id', id, 'name', name)
	from categories
	where 
		not id = any (select * from par)
		and pid is null
),
t1 as (
	select
		pid,
		jsonb_agg(jsonb_build_object('id', id, 'name', name, 'subcat', ''))
	from categories
	where 
		not id = any (select * from par)
		and not pid is null
	group by
		pid
),
lvls as (
	select id, name, pid, 0 as lvl
		from categories
	where pid is null
	
union all
	
	select c.id, c.name, c.pid, lvl+1
		from lvls l
			join categories c
				on c.pid = l.id
),

max_lvl as (
	select max(lvl) as lvl
		from lvls
),

level_2 as (
	select pid, jsonb_agg(jsonb_build_object('id', id, 'name', name, 'pid', pid, 'subcat', null)) as js
		from lvls
	where lvl in (select lvl from max_lvl)
	group by pid
),

level_1 as (
	select l.pid, jsonb_agg(jsonb_build_object('id', l.id, 'name', l.name, 'pid', l.pid, 'subcat', l2.js)) as js
		from lvls as l
			left join level_2 as l2
				on l.id = l2.pid
	where lvl = 1
	group by l.pid
),

level_0 as (
	select l.pid, jsonb_agg(jsonb_build_object('id', l.id, 'name', l.name, 'pid', l.pid, 'subcat', l2.js))
		from lvls as l
			left join level_1 as l2
				on l.id = l2.pid
	where lvl = 0
	group by l.pid
),

lowest_level as (
	select id, name, pid
		from lvls
	where lvl in (select lvl from max_lvl)
)


select * from level_0