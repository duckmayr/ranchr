library(dplyr)
library(tidyr)
library(readr)
baselines = read_csv("inst/extdata/Monster_baselines.csv") %>%
    select(monster, main, sub)
statgains = read_csv("inst/extdata/Monster_statgains.csv") %>%
    select(monster, main, sub)
monsterlist = as.data.frame(inner_join(baselines, statgains))
# usethis::use_data(monsterlist, overwrite = TRUE)

baselines = read_csv("inst/extdata/Monster_baselines.csv")
statgains = read_csv("inst/extdata/Monster_statgains.csv")

foo = function(monster_) {
    tmp = baselines %>% filter(monster == monster_)
    xxx = c(
        life = tmp$life,
        power = tmp$power,
        intelligence = tmp$intelligence,
        skill = tmp$skill,
        speed = tmp$speed,
        defense = tmp$defense
    )
    tmp = statgains %>% filter(monster == monster_)
    yyy = c(
        life = tmp$life,
        power = tmp$power,
        intelligence = tmp$intelligence,
        skill = tmp$skill,
        speed = tmp$speed,
        defense = tmp$defense
    )
    return(structure(
        .Data = list(
            name = paste(monster_, "Baseline"),
            monster = monster_,
            main = tmp$main,
            sub = tmp$sub,
            stats = xxx,
            baselines = xxx,
            statgains = yyy,
            lifespan = tmp$lifespan
        ),
        class = c("Monster", "list")
    ))
}

monsters = lapply(monsterlist$monster, foo)
names(monsters) = monsterlist$monster
# usethis::use_data(monsters, overwrite = TRUE)
