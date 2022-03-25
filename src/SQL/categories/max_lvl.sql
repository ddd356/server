with recursive lvls as (
    select id, name, pid, 0 as lvl
        from categories
    where pid is null
    
union all
    
    select c.id, c.name, c.pid, lvl+1
        from lvls l
            join categories c
                on c.pid = l.id
)
SELECT max(lvl) as lvl
    FROM lvls
