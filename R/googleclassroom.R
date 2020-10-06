#' Authenticate user and create classroom service
#'
#' @return
#' @export
#'
#' @examples cs <- classroomService()
classroomService <- function(){
  classroom_auth()
  list(
    list_activeCourses=list_activeCourses,
    list_studentsInACourse=list_studentsInACourse,
    list_teachersInACourse=list_teachers,
    create_attendance=create_attendance
  )

}

classroom_auth <- function(){
  .auth <<- gargle::init_AuthState(
    package     = "googleclassroom",
    auth_active = TRUE
    # app = NULL,
    # api_key = NULL,
    # cred = NULL
  )
  classroom_tokenFetch()
}

classroom_scopes <- function(){
  c(
    "https://www.googleapis.com/auth/classroom.courses",
    "https://www.googleapis.com/auth/classroom.rosters",
    "https://www.googleapis.com/auth/classroom.coursework.me",
    "https://www.googleapis.com/auth/classroom.announcements",
    "https://www.googleapis.com/auth/classroom.profile.photos",
    "https://www.googleapis.com/auth/classroom.profile.emails",
    "https://www.googleapis.com/auth/classroom.coursework.students"
  )
}


classroom_tokenFetch <- function(email = gargle::gargle_oauth_email(),
                           path = NULL,
                           scopes = classroom_scopes(),
                           cache = gargle::gargle_oauth_cache(),
                           use_oob = gargle::gargle_oob_default(),
                           token = NULL) {
  cred <- gargle::token_fetch(
    scopes = scopes,
    app = httr::oauth_app(
      appname = "classroom",
      key = "808460346772-p9cj09ss5ucrnmffh7bkf5pspnu29al3.apps.googleusercontent.com",
      secret = "uOwQ5PDbKLsA4CCWC3Mkim83"),
    email = email,
    path = path,
    package = "googleclassroom",
    cache = cache,
    use_oob = use_oob,
    token = token
  )

  if (!inherits(cred, "Token2.0")) {
    # throw an informative error here
  }
  .auth$set_cred(cred)
  .auth$set_auth_active(TRUE)

  invisible()
}

get_verb_url <- function(requestPhrase){
  unlist(stringr::str_split(requestPhrase,"\\s")) ->result
  names(result) <- c("VERB","url")
  result
}

# courseId <- "141372606074"
# requestPhrase <- glue::glue("GET https://classroom.googleapis.com/v1/courses/{courseId}/teachers")
list_teachers <- function(courseId){
  requestPhrase <- glue::glue("GET https://classroom.googleapis.com/v1/courses/{courseId}/teachers")
  list_teachersFun <- classroom_apiFunctional(requestPhrase)
  list_teachersFun()
}
classroom_apiFunctional <- function(requestPhrase){
  result=get_verb_url(requestPhrase)
  VERB=result[["VERB"]]
  classroomUrl=result[["url"]]
  function(...){
    group_vars = rlang::enquos(...)
    require(httr)
    requestExpr=rlang::quo({
      (!!VERB)(
        url=classroomUrl,
        config=httr::config(token=.auth$cred),
        !!!group_vars
      )
    })

    response <- rlang::eval_tidy(
      requestExpr
    )

    content(response)
  }
}

# courseId="141372606074"
# .title="09-23 attendance"
# .maxPoints=10
# .description="當天有完成fork"
create_attendance <- function(courseId, .title,.description="", .maxPoints=10){
  classroom_auth()
  requestPhrase= glue::glue("POST https://classroom.googleapis.com/v1/courses/{courseId}/courseWork")

  create_attendanceFun = classroom_apiFunctional(requestPhrase = requestPhrase)
  create_attendanceFun(
    body=jsonlite::toJSON(
      list(
        title=.title,
        maxPoints=.maxPoints,
        description=.description,
        workType="ASSIGNMENT"
      ), auto_unbox = T
    )
  ) -> response
  response
}


list_activeCourses <- function(){
  requestPhrase = "GET https://classroom.googleapis.com/v1/courses"
  list_courses <- classroom_apiFunctional(requestPhrase = requestPhrase)
  list_courses(query=list(
    courseStates="ACTIVE"
  ))
}

list_studentsInACourse <- function(courseId){
  requestPhrase = paste0("GET https://classroom.googleapis.com/v1/courses/",courseId,"/students")
  list_students <- classroom_apiFunctional(requestPhrase = requestPhrase)
  firstPage <- list_students(query=list(
    pageSize=100
  ))
  students <- firstPage$students
  nextPageToken <- firstPage$nextPageToken
  count=0; max_count=10
  while(length(firstPage)==2 && count <=max_count){
    newPage <- list_students(query=list(
      pageSize=100,
      pageToken=nextPageToken
    ))
    append(students, newPage$students) -> students
    nextPageToken <- newPage$nextPageToken
    firstPage=newPage
  }
  students
}
