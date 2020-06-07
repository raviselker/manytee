#' @importFrom magrittr %>%
manyttestISClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "manyttestISClass",
    inherit = manyttestISBase,
    private = list(
        ### Member variables ----
        .groups = NULL,
        .comparisons = NULL,
        
        #### Init + run functions ----
        .init = function() {
            
            private$.initTable()
            
        },
        .run = function() {
            
            if (is.null(self$options$dep) || length(self$options$groups) == 0)
                return()
            
            results <- private$.compute()
            
            private$.populateTable(results)
        },
        
        #### Compute results ----
        .compute = function() {
            
            dep <- self$options$dep
            vars <- self$options$groups
            
            levels <- self$groups %>%
                tidyr::unite(group, vars, sep = '...') %>%
                .[['group']]
            
            data <- self$data %>%
                tibble::as_tibble() %>%
                dplyr::select(c(dep, vars)) %>%
                tidyr::unite(group, vars, sep = '...') %>%
                dplyr::mutate(group = factor(group, levels))
            
            results <- private$.ttests(data)
            
            return(results)
        },
        
        #### Init table ----
        .initTable = function() {
            
            table <- self$results$tests
            groupNames <- self$options$groups
            
            for (i in 1:2) {
                for (name in groupNames) {
                    table$addColumn(
                        name=paste0(name, i), 
                        title=name, type='text', 
                        superTitle=paste('Group', i),
                        combineBelow=i==1)
                }

                table$addColumn(
                    name=paste0('n', i), 
                    title='N', 
                    type='number', 
                    superTitle=paste('Group', i),
                    visible="(n)")
            }
            
            table$addColumn(name='t', title='t', type='number')
            table$addColumn(name='df', title='df', type='number')
            table$addColumn(name='p', title='p', type='number', format='zto,pvalue')
            table$addColumn(name='d', title='Cohen\'s d', type='number', visible="(effectSize)")
            
            groups <- self$groups
            levels <- groups %>%
                tidyr::unite(group, groupNames, sep = '...') %>%
                .[['group']]
            nGroups <- nrow(groups)
            
            if (is.null(groups))
                return()
            
            comp <- tibble::tibble(group1=character(), group2=character())
            for (i in 1:(nGroups-1)) {
                for (j in (i+1):nGroups) {
                    
                    row <- list()
                    for (name in groupNames) {
                        row[[paste0(name, 1)]] <- as.character(groups[i,name])
                        row[[paste0(name, 2)]] <- as.character(groups[j,name])
                    }
                    table$addRow(rowKey=paste0(i,j), row)
                    
                    comp <- comp %>%
                        tibble::add_row(group1=levels[i], group2=levels[j])
                }
            }
            private$.comparisons <- comp
        },
        
        #### Populate table ----
        .populateTable = function(results) {
            
            table <- self$results$tests
            
            print(results)

            for (i in 1:nrow(results)) {
                row <- as.list(results[i,])
                table$setRow(rowNo=i, values=row)
            }
        },
        
        #### Helper functions ----
        .ttests = function(data) {
            
            comp <- private$.comparisons
            dep <- self$options$dep
            vars <- self$options$groups
            
            ts <- tibble::tibble(group1=character(), group2=character(),
                                 n1=integer(), n2=integer(),
                                 t=numeric(), df=numeric(), p=numeric(), 
                                 ci_lower=numeric(), ci_upper=numeric())
            
            for (i in 1:nrow(comp)) {
                
                group1 <- data %>% 
                    dplyr::filter(group==!!as.character(comp[i,1])) %>%
                    dplyr::select(!!dep)
                
                group2 <- data %>% 
                    dplyr::filter(group==!!as.character(comp[i,2])) %>%
                    dplyr::select(!!dep)
                
                n1 <- nrow(group1)
                n2 <- nrow(group2)
                
                if (n1 < 1 || n2 < 1 || n1 + n2 <= 2) {
                    
                    ts <- ts %>% tibble::add_row(
                        group1=as.character(comp[i,1]), group2=as.character(comp[i,2]),
                        n1=!!n1, n2=!!n2, t=NaN, df=NaN, p=NaN, ci_lower=NaN, ci_upper=NaN)
                    
                } else {
                    
                    test <- t.test(group1, group2, var.equal = TRUE)
                    
                    ts <- ts %>% tibble::add_row(
                        group1=as.character(comp[i,1]), group2=as.character(comp[i,2]),
                        n1=!!n1, n2=!!n2, t=test$statistic, df=test$parameter,
                        p=test$p.value, ci_lower=test$conf.int[1], ci_upper=test$conf.int[2])
                }
            }
            
            ts <- ts %>%
                dplyr::mutate(p_adjust = stats::p.adjust(p, method = 'holm'))
            
            # ts <- ts %>% 
            #     tidyr::separate(group1, paste(vars, 1, sep='_'), sep="\\.{3}") %>%
            #     tidyr::separate(group2, paste(vars, 2, sep='_'), sep="\\.{3}")
            
            ts <- ts %>% dplyr::select(-c(group1, group2))
            
            return(ts)
        }
    ),
    active = list(
        ### Active binding for groups ----
        groups = function(value) {
            if (missing(value)) {

                if (is.null(private$.groups)) {
                    
                    groups <- self$options$groups
                    
                    if (length(groups) == 0)
                        return(NULL)
                    
                    levels <- list()
                    for (group in groups)
                        levels[[group]] <- levels(self$data[[group]])
                    
                    private$.groups <- rev(expand.grid(rev(levels)))
                    
                }
                    
                return(private$.groups)
            }
        }
    )
)
