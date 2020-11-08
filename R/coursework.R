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
cat('downloading:\n')
problemIndex <- c()
for (.x in seq_along(df_studentSubmissions$attachments)) {
  if (is.null(df_studentSubmissions$attachments[[.x]])) next
  cat('.x= ', .x, "\n")
  filename <- paste(title,
    df_studentSubmissions[.x, ]$學號,
    sep = "_"
  ) %>%
    stringr::str_remove_all("\\s")

  filename <- paste0(filename, ".Rmd")
  drivePath <- df_studentSubmissions[.x, ]$attachments[[1]]$driveFile$alternateLink
  if(is.null(drivePath)) {
    problemIndex <- c(problemIndex, .x)
    next
  }
  drive_download(
    file = drivePath,
    path = file.path(destPath, filename),
    overwrite = T
  )
}
  list(
    failed_cases = df_studentSubmissions[problemIndex,],
    success_downloads=list.files(destPath)
  ) -> results
  list(
    results
    ) -> listOfDownloadedRmds
  names(listOfDownloadedRmds) <- title
  invisible(listOfDownloadedRmds)
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
        dueDate={.x$dueDate %>% unlist() %>% as.character() %>% paste(collapse="-")},
        dueTime={
          .x$dueTime%>% unlist() %>% as.character()  -> dueTime
          dueTime
          timeTemplate <- c("00","00","00")
          timeTemplate[seq(dueTime)] <- dueTime
          timeTemplate %>% paste(collapse = ":")}
        )
    }) %>%
    mutate(
      due=lubridate::ymd_hms(paste0(dueDate,"T",dueTime,"Z")),
      due=lubridate::with_tz(due, tzone='Asia/Taipei')
    )
}


# helpers -----------------------------------------------------------------


get_oneCoursework <- function(courseId, courseWorkId){
  requestPhrase <-
    glue::glue("GET https://classroom.googleapis.com/v1/courses/{courseId}/courseWork/{courseWorkId}")
  requestPhrase
  get_oneCourseworkFun <- classroom_apiFunctional(requestPhrase)
  courseworkInfo <- get_oneCourseworkFun()
  courseworkInfo
}

