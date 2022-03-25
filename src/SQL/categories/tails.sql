level_?1 as (
	select l.pid, jsonb_agg(jsonb_build_object('id', l.id, 'name', l.name, 'pid', l.pid, 'subcat', l?2.js)) as js
		from lvls as l
			left join level_?2 as l?2
				on l.id = l?2.pid
	where lvl = ?1
	group by l.pid
)
