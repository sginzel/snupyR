# sc = SampleCollection$new(c(4,5,6, 3269), ssion)
# sample_collection = sc
### not sure what this file can contain.
#'@export snupy.find_outliers
snupy.find_outliers <- function(sample.collection, dist.mat = NULL, create.plot = F){
	# get distance matrix of samples collection
	# dist.mat = sample_collection$distance.matrix(measure = "cosine", attribute = "baf", min.dp = 0, col.row.name = "id")
	# remove samples that do not have a specimen_probe id
	sample_collection = sample.collection$each("field", "specimen_probe_id") %>% sapply(., function(x){!is.null(x)}) %>% sample.collection$filter(.)
	all.sample_ids = sample.collection$ids

	if (dist.mat %>% is.null)
		dist.mat = sample_collection$distance.matrix(measure = "cosine", attribute = "present", min.dp = 30, col.row.name = "id")

	outliers = list()
	# for each entity
	ents = sample_collection$each("entities")
	stop("not implemtend correclty.")
	# TODO: The idea is correct, but the condition fails if a wrongly assigned sample makes max(ent_dist) really large
	for (ent in ents){
		ent_sample_ids = ent$each("samples")[[1]]$ids %>% as.character
		ent_group_sample_ids = ent$each("entity_groups")[[1]]$each("samples")[[1]]$ids %>% as.character
		ent_group_other_samples = setdiff(ent_group_sample_ids, ent_sample_ids)
		ent_foreign = setdiff(rownames(dist.mat), ent_group_sample_ids)

		ent_sample_ids = ent_sample_ids[ent_sample_ids %in% all.sample_ids]
		ent_group_sample_ids = ent_group_sample_ids[ent_group_sample_ids %in% all.sample_ids]
		ent_group_other_samples = ent_group_other_samples[ent_group_other_samples %in% all.sample_ids]
		ent_foreign = ent_foreign[ent_foreign %in% all.sample_ids]


		#   check if the distance between the entity and the other members of its entity group is smaller than the distance to all other samples
		is.ok = T
		if (length(ent_sample_ids) > 0 & length(ent_group_other_samples) > 0 & length(ent_foreign) > 0){
			ent_dist = dist.mat[ent_sample_ids , ent_group_other_samples]
			ent_dist_foreign = dist.mat[ent_sample_ids , ent_foreign]
			is.ok = max(ent_dist) < min(ent_dist_foreign)
		}
		if (!is.ok){
			outliers[as.character(ent$ids)]
		}
	}

	if (create.plot){
		lbls = sample_collection$fields("nickname")
		names(lbls) = sample_collection$fields("id") %>% as.character
		lbls = lbls[rownames(dist.mat)]
		lbls[unlist(outliers)] = paste0("(OUTLIER)", lbls[unlist(outliers)])
		plot(hclust(as.dist(dist.mat)), hang=-1, labels=unlist(lbls))
		tmp = sample_collection$distance.matrix(measure = "cosine", attribute = "present", min.dp = 30, col.row.name = "nickname")
	}

	outliers
}
