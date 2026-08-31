## ---------------------------------------------------------------
## submit_check.R  —  제출 전 자가점검
## 응용프로그래밍:R  2026-2학기
##
## 각 주차 Rmd 문서 맨 끝의 자가점검 청크에서 부릅니다.
##   source("submit_check.R")
##   submit_check(week = 1)
##
## Knit 결과 맨 끝에 점검표가 찍힙니다.
## 통과하지 못한 항목은 무엇을 고쳐야 하는지 함께 알려 줍니다.
## 형식만 봅니다. 내용의 깊이는 교수자가 읽고 평가합니다.
## ---------------------------------------------------------------

submit_check <- function(week, file = NULL, env = parent.frame()) {

  ## ---- 문서 원본 읽기 --------------------------------------------
  if (is.null(file)) {
    file <- tryCatch(knitr::current_input(), error = function(e) NA_character_)
  }
  src <- character(0)
  if (length(file) == 1 && !is.na(file) && file.exists(file)) {
    src <- readLines(file, warn = FALSE, encoding = "UTF-8")
  }
  ## 자가점검 청크 자체는 검사 대상에서 제외
  cut <- grep("submit_check\\s*\\(", src)
  if (length(cut)) {
    stop_at <- max(1L, min(cut) - 3L)
    src <- src[seq_len(stop_at)]
  }

  ## ---- 결과표 --------------------------------------------------
  res <- data.frame(check = character(), result = character(),
                    fix = character(), stringsAsFactors = FALSE)
  add <- function(label, pass, hint = "") {
    res[nrow(res) + 1L, ] <<- c(label,
                                if (isTRUE(pass)) "통과" else "확인 필요",
                                if (isTRUE(pass)) "" else hint)
  }

  ## ---- 1. 제출자 표기 --------------------------------------------
  au <- grep("^\\s*author\\s*:", src, value = TRUE)
  au <- if (length(au)) au[1] else ""
  add("제출자 표기",
      grepl("[0-9]{8}", au) && !grepl("coop711|Your Name|홍길동|제출자", au),
      "YAML 의 author 를 '데이터과학부, 학번 8자리, 이름' 으로 바꾸세요.")

  ## ---- 2. Comments 네 항목 ---------------------------------------
  keys <- c("맥락", "주장", "근거", "한계")
  find_head <- function(k) {
    i <- grep(paste0("[1-4]\\s*[.)]\\s*\\**\\s*", k), src)
    if (length(i)) i[1] else NA_integer_
  }
  head_at <- vapply(keys, find_head, integer(1))
  ## 절 제목(##...)과 페이지 나눔은 Comments 구역의 끝으로 본다
  stop_marks <- sort(c(grep("^\\s*#{1,6}\\s", src),
                       grep("^\\s*<P[ >]", src, ignore.case = TRUE)))
  for (i in seq_along(keys)) {
    from <- head_at[i]
    if (is.na(from)) {
      add(paste0("Comments ", i, ". ", keys[i]), FALSE,
          "이 항목의 제목이 문서에 없습니다. 양식의 제목 줄을 지우지 마세요.")
      next
    }
    nxt <- c(head_at[!is.na(head_at) & head_at > from],
             stop_marks[stop_marks > from])
    to  <- if (length(nxt)) min(nxt) - 1L else length(src)
    body <- src[from:to]
    ## 제목 줄에 붙어 있는 첫 문단(= 문제 지문)은 검사 대상에서 뺀다.
    ## 학생이 쓰는 자리는 '(여기에 작성)' 이 있던 그 아래이기 때문.
    j <- 1L
    while (j <= length(body) && nzchar(trimws(body[j]))) j <- j + 1L
    body <- if (j < length(body)) body[(j + 1L):length(body)] else character(0)
    body <- body[!grepl("^\\s*```", body)]
    body <- body[!grepl("^\\s*>", body)]
    body <- body[!grepl("^\\s*<!--|-->", body)]
    body <- body[!grepl("여기에 작성", body)]
    n <- sum(nchar(gsub("[[:space:]]", "", body)))
    add(paste0("Comments ", i, ". ", keys[i]), n >= 40,
        "비어 있거나 너무 짧습니다. '(여기에 작성)' 을 지우고 두세 문장으로 쓰세요.")
  }

  ## ---- 3. 주차별 필수 계산 ---------------------------------------
  got <- function(x) exists(x, envir = env, inherits = TRUE)
  val <- function(x) get(x, envir = env, inherits = TRUE)
  safe <- function(expr) isTRUE(tryCatch(expr, error = function(e) FALSE))

  if (week == 1) {
    add("필수 계산 1 : VA_colored (20행)",
        safe(got("VA_colored") && is.data.frame(val("VA_colored")) &&
             nrow(val("VA_colored")) == 20),
        "Colored 네 열을 50-54 ~ 70-74 다섯 연령군만 긴 형태로 펴서 VA_colored 에 담으세요. 5 x 4 = 20 행입니다.")
    add("필수 계산 2 : g_colored",
        safe(got("g_colored")),
        "VA_colored 를 VADeaths 와 같은 방식으로 그린 막대그래프를 g_colored 에 담으세요.")

  } else if (week == 2) {
    add("필수 계산 1 : 이란성 31쌍",
        safe(got("N1") && sum(val("N1")["Fraternal", ]) == 31),
        "N1 의 Fraternal 행이 11, 4, 16 이라서 합계 31 이 되어야 합니다. 33 이 나오면 원문과 다릅니다.")
    add("필수 계산 2 : 닮지 않음 51.6%",
        safe(got("pct_unlike") &&
             abs(as.numeric(val("pct_unlike")["Fraternal"]) - 51.6) < 0.2),
        "행별 '닮지 않음' 백분율을 pct_unlike 에 담으세요. Fraternal 이 51.6 이어야 피셔 원문과 맞습니다.")

  } else if (week == 3) {
    ok <- safe(got("gof_two") &&
               nrow(as.data.frame(val("gof_two"))) == 2 &&
               "p_value" %in% names(as.data.frame(val("gof_two"))))
    add("필수 계산 1 : 두 판본 적합도", ok,
        "케틀레 원표와 스티글러 정정본의 적합도 검정 결과를 2행으로 묶어 gof_two 에 담으세요. p_value 열이 있어야 합니다.")
    if (ok) {
      p <- as.numeric(as.data.frame(val("gof_two"))$p_value)
      add("필수 계산 2 : 결론이 갈리는가",
          safe(p[1] < 0.05 && p[2] > 0.05),
          "케틀레 원표는 기각(p < 0.05), 스티글러 정정본은 기각되지 않아야(p > 0.05) 합니다. 구간 묶기를 다시 확인하세요.")
    }
  } else if (week == 4) {
    ok1 <- safe({
      x <- val("crimtab_in")
      cn <- suppressWarnings(as.numeric(colnames(x)))
      got("crimtab_in") && length(cn) == 22 && all(!is.na(cn)) &&
        all(cn == 56:77)
    })
    add("필수 계산 1 : crimtab_in (인치)", ok1,
        "crimtab 의 열 이름을 2.54 로 나눠 되돌린 표를 crimtab_in 에 담으세요. 열 이름이 56 부터 77 까지 정수여야 합니다.")
    add("필수 계산 2 : r_fh (상관계수)",
        safe(got("r_fh") && is.numeric(as.numeric(val("r_fh"))) &&
             abs(as.numeric(val("r_fh"))[1] - 0.6557) < 0.01),
        "표를 개체 단위 3000행으로 편 뒤 손가락 길이와 키의 상관계수를 r_fh 에 담으세요. 0.66 근처(0.656)가 나와야 합니다.")

  } else if (week == 5) {
    ok1 <- safe({
      v <- val("overplot")
      nm <- names(v)
      got("overplot") && length(v) == 3 &&
        all(c("n_point", "n_spot", "n_max") %in% nm) &&
        as.numeric(v[["n_point"]]) == 3000 &&
        as.numeric(v[["n_spot"]])  == 301 &&
        as.numeric(v[["n_max"]])   == 58
    })
    add("필수 계산 1 : overplot (겹침 세기)", ok1,
        "n_point = 3000, n_spot = 301, n_max = 58 이 되어야 합니다. n_spot 은 nrow(unique(...)), n_max 는 max(table(...)) 로 구합니다. 이름 세 개를 그대로 붙여 주세요.")

    ok2 <- safe({
      g <- val("g_honest")
      if (!inherits(g, "ggplot")) FALSE else {
        cls <- function(x) tryCatch(class(x)[1], error = function(e) "")
        sts <- vapply(g$layers, function(l) cls(l$stat), character(1))
        gms <- vapply(g$layers, function(l) cls(l$geom), character(1))
        ## (가) 개수를 스스로 세는 stat / geom 을 쓴 경우
        cnt_stat <- any(sts %in% c("StatSum", "StatBin2d", "StatBin_2d",
                                   "StatBinhex", "StatSummary2d",
                                   "StatDensity2d", "StatDensity2dFilled")) ||
                    any(gms %in% c("GeomTile", "GeomRaster", "GeomHex"))
        ## (나) 직접 집계한 자료에 개수를 미적요소로 매핑한 경우
        nrows <- suppressWarnings(vapply(
          c(list(g$data), lapply(g$layers, function(l) l$data)),
          function(z) tryCatch(nrow(as.data.frame(z)), error = function(e) NA_integer_),
          integer(1)))
        aggd <- any(!is.na(nrows) & nrows > 0 & nrows <= 924)
        aes_all <- c(names(g$mapping),
                     unlist(lapply(g$layers, function(l) names(l$mapping))))
        cnt_aes <- any(c("size", "fill", "alpha") %in% aes_all)
        cnt_stat || (aggd && cnt_aes)
      }
    })
    add("필수 계산 2 : g_honest (개수가 보이는 그림)", ok2,
        "각 자리의 인원수가 그림에 나타나야 합니다. geom_count(), geom_bin2d(), geom_hex() 중 하나를 쓰거나, 개수를 세어 aes(size = n) 으로 매핑하세요. alpha 만 준 geom_point 는 통과하지 못합니다.")

  }

  ## ---- 4. 출력 ---------------------------------------------------
  names(res) <- c("점검 항목", "결과", "고칠 점")
  bad <- res[["결과"]] != "통과"
  msg <- if (any(bad)) paste0("제출 전에 ", sum(bad), "가지를 고쳐야 합니다.")
         else "형식 점검을 모두 통과했습니다. 제출하세요."

  ## Knit 중이면 HTML 상자로, 콘솔에서 부르면 그냥 표로
  if (!isTRUE(getOption("knitr.in.progress"))) {
    cat("\n", msg, "\n\n", sep = "")
    out <- res[, c("결과", "점검 항목", "고칠 점")]
    print(format(out, justify = "left"), row.names = FALSE, right = FALSE)
    cat("\n이 점검은 형식만 봅니다. 내용의 깊이는 교수자가 읽고 평가합니다.\n")
    return(invisible(res))
  }

  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;",  x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  brd <- if (any(bad)) "#c0392b" else "#1e7a4c"
  bg  <- if (any(bad)) "#fdf1ee" else "#eef7f1"
  fg  <- if (any(bad)) "#a03014" else "#14603a"

  rows <- character(nrow(res))
  for (i in seq_len(nrow(res))) {
    okc <- if (bad[i]) "#c0392b" else "#1e7a4c"
    rows[i] <- paste0(
      '<tr>',
      '<td style="padding:5px 8px; border-top:1px solid #e1e0d9; white-space:nowrap; ',
      'color:', okc, '; font-weight:bold">', esc(res[i, 2]), '</td>',
      '<td style="padding:5px 8px; border-top:1px solid #e1e0d9">', esc(res[i, 1]), '</td>',
      '<td style="padding:5px 8px; border-top:1px solid #e1e0d9; color:#52514e">',
      esc(res[i, 3]), '</td></tr>')
  }

  cat("\n")
  cat('<div style="border:2px solid ', brd, '; background:', bg,
      '; padding:14px 18px; border-radius:4px">\n', sep = "")
  cat('<p style="margin:0 0 10px; color:', fg,
      '; font-weight:bold; font-size:15px">', msg, '</p>\n', sep = "")
  cat('<table style="width:100%; border-collapse:collapse; font-size:13px">\n',
      paste(rows, collapse = "\n"), '\n</table>\n', sep = "")
  cat('<p style="margin:10px 0 0; font-size:12px; color:#666">',
      "이 점검은 형식만 봅니다. 내용의 깊이는 교수자가 읽고 평가합니다.", "</p>\n", sep = "")
  cat("</div>\n")

  invisible(res)
}
