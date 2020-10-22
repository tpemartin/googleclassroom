load("/Users/martinl/Github/course-dashboard-programming-for-data-science/course109-1-program4DS.rda")

courseId = courseInfo$resources$googleClassroom$courseInfo$id

# list all courseworks
courseId %>% list_courseworksAsDataframe() -> df_courseworks
# get target coursework
courseWorkId = df_courseworks$id[[1]]
title = df_courseworks$title[[1]]

# get student submissions for a course work
load("~/Github/course-dashboard-programming-for-data-science/109-1-roster.rda")
get_studentSubmissionForACourseWork(courseWorkId, roster) -> df_studentSubmissions

# download student submission Rmds
download_studentRmds(title, df_studentSubmissions)

get_oneCoursework(courseId, courseWorkId) -> courseworkInfo

#' Download student Rmd submissions
#'
#' @param title A character. The title of the course work. It will be used as a folder name for this course work
#' @param df_studentSubmissions A data frame. Can be the return from get_studentSubmissionForACourseWork
#'
#' @return
#' @export
#'
#' @examples none
download_studentRmds <- function(title, df_studentSubmissions)
{
  destPath = file.path("studentsSubmission", title) %>%
    stringr::str_remove_all("\\s")
  if(!dir.exists(destPath)) dir.create(destPath, recursive = T)
  require(googledrive)

for (.x in seq_along(df_studentSubmissions$attachments)) {
  if (is.null(df_studentSubmissions$attachments[[.x]])) next
  filename <- paste(title,
    df_studentSubmissions[.x, ]$學號,
    sep = "_"
  ) %>%
    stringr::str_remove_all("\\s")

  filename <- paste0(filename, ".Rmd")
  drive_download(
    file = df_studentSubmissions[.x, ]$attachments[[1]]$driveFile$alternateLink,
    path = file.path(destPath, filename),
    overwrite = T
  )
}

}
get_oneCoursework <- function(courseId, courseWorkId){
  requestPhrase <-
    glue::glue("GET https://classroom.googleapis.com/v1/courses/{courseId}/courseWork/{courseWorkId}")
  requestPhrase
  get_oneCourseworkFun <- classroom_apiFunctional(requestPhrase)
  courseworkInfo <- get_oneCourseworkFun()
  courseworkInfo
}
#' Get student submission for a course work
#'
#' @param courseWorkId A character. can be chosen from the return of list_courseworksAsDataframe
#' @param roster A data frame which has 姓名 學號 googleUserId
#'
#' @return
#' @export
#'
#' @examples none
get_studentSubmissionForACourseWork <- function(courseWorkId, roster)
{
  requestPhrase <-
    glue::glue("GET https://classroom.googleapis.com/v1/courses/{courseId}/courseWork/{courseWorkId}/studentSubmissions")
  requestPhrase
  get_aCourseworkStudentSubmissionFun <- classroom_apiFunctional(requestPhrase)
  studentSubmissionInfo <- get_aCourseworkStudentSubmissionFun()
  studentSubmissionInfo$studentSubmissions %>%
    purrr::map_dfr(
      ~{
        tidyr::tibble(
          userId=.x$userId,
          id=.x$id,
          state=.x$state,
          attachments={
            .x[["assignmentSubmission"]][["attachments"]] -> att
            ifelse(is.null(att), list(), att)
            },
          submissionTime={
            .x$submissionHistory %>% length() -> loc_last
            .x$submissionHistory[[loc_last]]$stateHistory$stateTimestamp
          },
          submissionState={
            .x$submissionHistory[[loc_last]]$stateHistory$state
          }

        )
      }) -> df_studentSubmissions

  roster %>%
    select("姓名","學號","googleUserId") %>%
    right_join(
      df_studentSubmissions,
      by=c("googleUserId"="userId")
    )
}
#' list all course works
#'
#' @param courseId
#'
#' @return
#' @export
#'
#' @examples none.
list_courseworksAsDataframe <- function(courseId)
{
  requestPhrase <- glue::glue(
    "GET https://classroom.googleapis.com/v1/courses/{courseId}/courseWork")
  requestPhrase
  list_courseworksFun <- classroom_apiFunctional(requestPhrase)
  list_courseworks <- list_courseworksFun()
  require(dplyr)
  list_courseworks$courseWork %>%
    purrr::map_dfr(
     ~{
      data.frame(
        title=.x$title,
        id=.x$id,
        dueDate={.x$dueDate %>% unlist() %>% paste(collapse="-")},
        dueTime={.x$dueTime %>% unlist() %>%
          stringr::str_pad(width=2, side="left", pad="0") %>%
          paste0(":00:00")}
        )
    }) %>%
    mutate(
      due=lubridate::ymd_hms(paste0(dueDate,"T",dueTime,"Z")),
      due=lubridate::with_tz(due, tzone='Asia/Taipei')
    )
}
