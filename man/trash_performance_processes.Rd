% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trash_performance_processes.R
\name{trash_performance_processes}
\alias{trash_performance_processes}
\title{trash_performance_processes Function}
\usage{
trash_performance_processes(df = NULL, var, query = F)
}
\arguments{
\item{df}{Data frame to filter}

\item{var}{Name of the df var to filter as string}

\item{query}{Boolean. Are you filtering inside the query?}
}
\description{
Filters test performance enterprise_process names from Rankmi Database
}
\examples{

# query = F
trash_performance_processes(df, "process_name")

# query = T
DBI::dbGetQuery(con, glue::glue("select ep.name_translate ->> 'es' as name_translate,
ep.name_old
from enterprise_processes ep
left join enterprises e on e.id = ep.enterprise_id
where rankmi_module_id = 1
{trash_performance_processes(var = 'ep.name_translate::text', query = T)}"))
}
