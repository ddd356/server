ALTER TABLE IF EXISTS categories
ADD CONSTRAINT parent_id_foreign FOREIGN KEY (parent_id)
REFERENCES public.categories (id) MATCH SIMPLE 
ON UPDATE NO ACTION 
ON DELETE NO ACTION 
NOT VALID; 
CREATE INDEX IF NOT EXISTS fki_parent_id_foreign 
ON public.categories(parent_id);
