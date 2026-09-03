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

  } else if (week == 6) {
    ok1 <- safe({
      v <- as.numeric(val("t750"))
      got("t750") && length(v) == 750 && !any(is.na(v)) &&
        !any(is.infinite(v)) && sd(v) > 1.2 && sd(v) < 2.4
    })
    add("필수 계산 1 : t750 (750개 t-값)", ok1,
        "sample_t 를 t750 에 담으세요. 750개여야 하고, 무한대를 +-6 으로 바꾼 뒤의 값이어야 합니다. 표준편차가 1.2~2.4 밖이면 t-값 계산을 다시 확인하세요.")

    ok2 <- safe({
      v <- val("tail_cmp")
      nm <- names(v)
      got("tail_cmp") && length(v) == 3 &&
        all(c("observed", "t_df3", "normal") %in% nm) &&
        abs(as.numeric(v[["t_df3"]])  - 0.1393) < 0.002 &&
        abs(as.numeric(v[["normal"]]) - 0.0455) < 0.002 &&
        as.numeric(v[["observed"]]) > 0.09 &&
        as.numeric(v[["observed"]]) < 0.20
    })
    add("필수 계산 2 : tail_cmp (꼬리 확률 비교)", ok2,
        "observed / t_df3 / normal 세 이름으로 담으세요. t_df3 = 2*pt(-2, 3) = 0.1393, normal = 2*pnorm(-2) = 0.0455 입니다. observed 가 0.09~0.20 밖이면 t-값이나 +-6 처리를 확인하세요.")

  } else if (week == 7) {
    ok1 <- safe({
      v <- val("trap_count")
      nm <- names(v)
      got("trap_count") && length(v) == 4 &&
        all(c("na_string", "sido_trim", "sido_raw", "zero_2012") %in% nm) &&
        as.numeric(v[["na_string"]]) == 48 &&
        as.numeric(v[["sido_trim"]]) == 8  &&
        as.numeric(v[["sido_raw"]])  == 9  &&
        as.numeric(v[["zero_2012"]]) == 82
    })
    add("필수 계산 1 : trap_count (함정 네 가지)", ok1,
        "na_string / sido_trim / sido_raw / zero_2012 네 이름으로 담으세요. 48 / 8 / 9 / 82 입니다. na_string 이 0 이면 is.na() 로 센 것이니 == \"NA\" 로 바꾸세요. sido_raw 가 8 이면 두 번째 읽기에 trim_ws = FALSE 를 빠뜨린 것입니다.")

    ok2 <- safe({
      v <- val("zero_split")
      nm <- names(v)
      got("zero_split") && length(v) == 2 &&
        all(c("not_yet", "existed") %in% nm) &&
        as.numeric(v[["not_yet"]]) == 72 &&
        as.numeric(v[["existed"]]) == 10
    })
    add("필수 계산 2 : zero_split (0 의 두 갈래)", ok2,
        "not_yet / existed 두 이름으로 담으세요. 72 / 10 입니다. 두 값을 더하면 82 여야 합니다. 안 맞으면 as.Date(\"2012-12-31\") 과 비교하는 부등호 방향을 보세요.")

  } else if (week == 8) {
    ok1 <- safe({
      g <- as.data.frame(val("gap_two"))
      if (!got("gap_two") || nrow(g) != 2 ||
          !all(c("with_zero", "without_zero") %in% names(g))) FALSE else {
        wz <- as.numeric(g[["with_zero"]]);  wo <- as.numeric(g[["without_zero"]])
        if (any(is.na(c(wz, wo))) || any(wz <= 0) || any(wo <= 0)) FALSE else
          abs(max(wz) / min(wz) - 50.19) < 0.6 &&
          abs(max(wo) / min(wo) -  3.52) < 0.10
      }
    })
    add("필수 계산 1 : gap_two (0 포함/제외 평균)", ok1,
        "with_zero / without_zero 두 열, 2행짜리 데이터 프레임으로 담으세요. 배율이 50.2배와 3.52배여야 합니다. 19.5배와 2.26배가 나왔다면 성과_매출_2014 를 쓴 것이니 매출_2014 로 바꾸세요.")

    ok2 <- safe({
      g <- as.data.frame(val("strata"))
      need <- c("n_startup", "pct_startup", "n_established", "pct_established")
      if (!got("strata") || nrow(g) != 3 || !all(need %in% names(g))) FALSE else {
        ns <- as.numeric(g[["n_startup"]]);     ne <- as.numeric(g[["n_established"]])
        ps <- as.numeric(g[["pct_startup"]]);   pe <- as.numeric(g[["pct_established"]])
        if (any(is.na(c(ns, ne, ps, pe)))) FALSE else
          sum(ns) == 70 && sum(ne) == 27 &&
          all(abs(ns - c(56, 11, 3))       < 0.5) &&
          all(abs(ne - c(3, 14, 10))       < 0.5) &&
          all(abs(ps - c(33.9, 63.6, 66.7)) < 0.3) &&
          all(abs(pe - c(100, 92.9, 80))    < 0.3)
      }
    })
    add("필수 계산 2 : strata (업력 세 층의 법인 비율)", ok2,
        "n_startup / pct_startup / n_established / pct_established 네 열, 3행(2년미만·2-5년·5년이상)으로 담으세요. 조직 수는 56/11/3 과 3/14/10 입니다. 57/10 이 나왔다면 cut() 에 right = FALSE 를 빠뜨린 것입니다.")

} else if (week == 9) {
    ok1 <- safe({
      g <- as.data.frame(val("xsum_summary"))
      need <- c("n", "min", "mean", "sd")
      if (!got("xsum_summary") || nrow(g) < 2 || !all(need %in% names(g))) FALSE else {
        기준 <- data.frame(n = c(100, 1000, 10000),
                           min = c(12.60, 9.44, 8.17),
                           mean = c(32.88, 32.26, 32.19),
                           sd = c(8.54, 8.19, 7.94))
        ok <- all(c(100, 1000) %in% as.numeric(g[["n"]]))
        for (r in seq_len(nrow(g))) {
          k <- match(as.numeric(g[["n"]][r]), 기준$n)
          if (is.na(k)) { ok <- FALSE; next }
          ok <- ok &&
            abs(as.numeric(g[["min"]][r])  - 기준$min[k])  < 0.02 &&
            abs(as.numeric(g[["mean"]][r]) - 기준$mean[k]) < 0.02 &&
            abs(as.numeric(g[["sd"]][r])   - 기준$sd[k])   < 0.02
        }
        ok
      }
    })
    add("필수 계산 1 : xsum_summary (반복횟수별 수렴)", ok1,
        "n / min / mean / sd 네 열로 담으세요. 100 회와 1,000 회는 반드시 있어야 합니다(10,000 회는 선택). 100 회는 12.60 / 32.88 / 8.54, 1,000 회는 9.44 / 32.26 / 8.19, 10,000 회는 8.17 / 32.19 / 7.94 입니다. 값이 다르면 시드를 1 부터 차례로 쓰지 않았거나 변수를 묶은 방식이 다른 것입니다.")

    ok2 <- safe({
      v <- val("best_five"); nm <- names(v)
      if (!got("best_five") || length(v) != 5 ||
          !all(c("학번", "이메일", "전화", "성씨", "단과대학") %in% nm)) FALSE else {
        기준 <- c(학번 = 0.764, 이메일 = 0.306, 전화 = 4.331,
                  성씨 = 0.967, 단과대학 = 6.233)
        all(abs(as.numeric(v[names(기준)]) - 기준) < 0.02)
      }
    })
    add("필수 계산 2 : best_five (가장 닮은 배정의 다섯 카이제곱)", ok2,
        "학번 / 이메일 / 전화 / 성씨 / 단과대학 다섯 이름으로 담으세요. 0.764 / 0.306 / 4.331 / 0.967 / 6.233 이고 합이 12.60 입니다. 합이 12.60 이 아니면 which.min 이 준 시드로 set.seed 를 다시 하지 않은 것입니다.")

  } else if (week == 10) {
    ok1 <- safe({
      g <- as.data.frame(val("share_layers"))
      need <- c("layer", "y1976", "y2024", "diff")
      if (!got("share_layers") || nrow(g) != 5 || !all(need %in% names(g))) FALSE else {
        기준 <- data.frame(
          layer = c("P90_95", "P95_99", "P99_100", "P99.9_100", "P99.99_100"),
          y1976 = c(11.4, 13.1,  8.9,  2.6, 0.9),
          y2024 = c(11.9, 17.3, 22.4, 11.3, 5.2),
          diff  = c( 0.5,  4.2, 13.5,  8.7, 4.3))
        k <- match(기준$layer, as.character(g[["layer"]]))
        if (any(is.na(k))) FALSE else
          all(abs(as.numeric(g[["y1976"]][k]) - 기준$y1976) < 0.06) &&
          all(abs(as.numeric(g[["y2024"]][k]) - 기준$y2024) < 0.06) &&
          all(abs(as.numeric(g[["diff"]][k])  - 기준$diff)  < 0.06)
      }
    })
    add("필수 계산 1 : share_layers (다섯 층의 1976 → 2024)", ok1,
        "layer / y1976 / y2024 / diff 네 열, 다섯 행으로 담으세요. diff 는 0.5 / 4.2 / 13.5 / 8.7 / 4.3 입니다. 값이 다르면 반올림을 먼저 하지 않았거나(round 를 뺀 값에 적용) 층 이름을 잘못 고른 것입니다.")

    ok2 <- safe({
      g <- as.data.frame(val("gap_1963"))
      need <- c("group", "y1963", "y2024", "ratio")
      if (!got("gap_1963") || nrow(g) != 2 || !all(need %in% names(g))) FALSE else {
        k <- match(c("Bottom99", "Top1"), as.character(g[["group"]]))
        if (any(is.na(k))) FALSE else
          abs(as.numeric(g[["ratio"]][k[1]]) - 1.53) < 0.02 &&
          abs(as.numeric(g[["ratio"]][k[2]]) - 3.91) < 0.02 &&
          abs(as.numeric(g[["y1963"]][k[1]]) - 44881) < 2 &&
          abs(as.numeric(g[["y2024"]][k[2]]) - 1542637) < 2
      }
    })
    add("필수 계산 2 : gap_1963 (1963년 대비 배율)", ok2,
        "group / y1963 / y2024 / ratio 네 열, 두 행(Bottom99, Top1)으로 담으세요. 배율은 1.53 과 3.91 입니다. 값이 다르면 data-FigA1 시트의 열을 잘못 고른 것입니다 — 2, 3 번째 열이 하위 99% 와 상위 1% 의 평균소득입니다.")

  } else if (week == 11) {
    ok1 <- safe({
      g <- as.data.frame(val("mtr_band"))
      need <- c("band", "n", "share")
      if (!got("mtr_band") || nrow(g) != 3 || !all(need %in% names(g))) FALSE else {
        기준 <- data.frame(band = c("under40", "40to70", "over70"),
                           n = c(49, 13, 50), share = c(19.3, 15.1, 11.7))
        k <- match(기준$band, as.character(g[["band"]]))
        if (any(is.na(k))) FALSE else
          all(as.integer(g[["n"]][k]) == 기준$n) &&
          all(abs(as.numeric(g[["share"]][k]) - 기준$share) < 0.06)
      }
    })
    add("필수 계산 1 : mtr_band (세율 구간별 평균 점유율)", ok1,
        "band / n / share 세 열, 세 행으로 담으세요. under40 은 49 해 19.3%, 40to70 은 13 해 15.1%, over70 은 50 해 11.7% 입니다. n 이 다르면 cut() 에 right = FALSE 를 빠뜨린 것입니다.")

    ok2 <- safe({
      g <- as.data.frame(val("z_check"))
      need <- c("Year", "share_mm", "tax_mm", "share_0", "tax_0")
      if (!got("z_check") || nrow(g) != 3 || !all(need %in% names(g))) FALSE else {
        기준 <- data.frame(Year = c(1944, 1976, 2024),
                           share_mm = c(0.129, 0.000, 0.724),
                           tax_mm   = c(1.000, 0.724, 0.345),
                           share_0  = c(0.113, 0.089, 0.224),
                           tax_0    = c(0.940, 0.700, 0.370))
        k <- match(기준$Year, as.numeric(g[["Year"]]))
        if (any(is.na(k))) FALSE else
          all(abs(as.numeric(g[["share_mm"]][k]) - 기준$share_mm) < 0.003) &&
          all(abs(as.numeric(g[["tax_mm"]][k])   - 기준$tax_mm)   < 0.003) &&
          all(abs(as.numeric(g[["share_0"]][k])  - 기준$share_0)  < 0.003) &&
          all(abs(as.numeric(g[["tax_0"]][k])    - 기준$tax_0)    < 0.003)
      }
    })
    add("필수 계산 2 : z_check (같은 해, 두 가지 기준)", ok2,
        "Year / share_mm / tax_mm / share_0 / tax_0 다섯 열, 세 행(1944·1976·2024)으로 담으세요. 1976 의 share_mm 은 0.000, share_0 은 0.089 입니다. 둘이 같게 나오면 z() 에 a 와 b 를 넘기지 않은 것입니다.")

  } else if (week == 12) {
    ok1 <- safe({
      g <- as.data.frame(val("pump_deaths"))
      need <- c("pump", "n", "share")
      if (!got("pump_deaths") || nrow(g) != 13 || !all(need %in% names(g))) FALSE else {
        k <- grep("Broad", as.character(g[["pump"]]))
        if (length(k) != 1) FALSE else
          sum(as.integer(g[["n"]])) == 578 &&
          as.integer(g[["n"]][k]) == 359 &&
          abs(as.numeric(g[["share"]][k]) - 62.1) < 0.06
      }
    })
    add("필수 계산 1 : pump_deaths (펌프별 최근접 사망자)", ok1,
        "pump / n / share 세 열, 열세 행으로 담으세요. 합이 578 이고 Broad St 가 359 명, 62.1% 입니다. 합이 578 이 아니면 max.col() 이 아니라 다른 방법으로 배정한 것입니다.")

    ok2 <- safe({
      g <- as.data.frame(val("edge_cases"))
      if (!got("edge_cases") || nrow(g) != 3 || !all(c("n", "share") %in% names(g))) FALSE else {
        기준_n <- c(77, 140, 249)
        기준_s <- c(13.3, 24.2, 43.1)
        all(as.integer(g[["n"]]) == 기준_n) &&
        all(abs(as.numeric(g[["share"]]) - 기준_s) < 0.06)
      }
    })
    add("필수 계산 2 : edge_cases (경계에 선 사람들)", ok2,
        "기준 / n / share 세 열, 세 행으로 담으세요. 위에서부터 77 명(13.3%), 140 명(24.2%), 249 명(43.1%) 입니다. 값이 다르면 거리 행렬 D 를 행별로 정렬(apply(D, 1, sort))하지 않은 것입니다.")

  } else if (week == 13) {
    ok1 <- safe({
      g <- as.data.frame(val("march_loss"))
      need <- c("구간", "시작", "끝", "손실", "손실률")
      if (!got("march_loss") || nrow(g) != 2 || !all(need %in% names(g))) FALSE else {
        all(as.numeric(g[["시작"]]) == c(340000, 100000)) &&
        all(as.numeric(g[["끝"]])   == c(100000,   4000)) &&
        all(as.numeric(g[["손실"]]) == c(240000,  96000)) &&
        all(abs(as.numeric(g[["손실률"]]) - c(70.6, 96.0)) < 0.06)
      }
    })
    add("필수 계산 1 : march_loss (진군과 후퇴의 손실)", ok1,
        "구간 / 시작 / 끝 / 손실 / 손실률 다섯 열, 두 행으로 담으세요. 진군 340000 \u2192 100000 (240000, 70.6%), 후퇴 100000 \u2192 4000 (96000, 96.0%) 입니다. 시작이 340000 이 아니거나 끝이 100000 이 아니면 본대 아닌 분견대까지 섞인 것입니다. group == 1 로 걸러내세요.")

    ok2 <- safe({
      g <- as.data.frame(val("temp_check"))
      if (!got("temp_check") || nrow(g) != 4 || !("값" %in% names(g))) FALSE else {
        v <- as.numeric(g[["값"]])
        v[1] == 9 && v[2] == 0 &&
        abs(v[3] - (-30)) < 1e-6 && abs(v[4] - (-37.5)) < 1e-6
      }
    })
    add("필수 계산 2 : temp_check (기온이 기록된 구간)", ok2,
        "항목 / 값 두 열, 네 행으로 담으세요. 위에서부터 9, 0, -30, -37.5 입니다. 두 번째가 0 이 아니면 관측 시기를 달 이름으로 세지 않은 것입니다. 진군 시기(6\u20139월)에는 기온 관측이 하나도 없습니다.")

  } else if (week == 14) {
    ok1 <- safe({
      g <- as.data.frame(val("match_dist"))
      need <- c("\ub9e4\uce6d\uc218", "\uacbd\uc6b0\uc758\uc218", "\uc774\ub860\ud655\ub960", "\ubaa8\uc758\ud655\ub960")
      if (!got("match_dist") || nrow(g) != 5 || !all(need %in% names(g))) FALSE else {
        cnt <- as.integer(g[["\uacbd\uc6b0\uc758\uc218"]])
        th  <- as.numeric(g[["\uc774\ub860\ud655\ub960"]])
        sm  <- as.numeric(g[["\ubaa8\uc758\ud655\ub960"]])
        all(cnt == c(9, 8, 6, 0, 1)) &&
        all(abs(th - c(9, 8, 6, 0, 1) / 24) < 0.001) &&
        sm[4] == 0 &&
        max(abs(sm - th)) < 0.01
      }
    })
    add("\ud544\uc218 \uacc4\uc0b0 1 : match_dist (\ub9e4\uce6d \uac1c\uc218\uc758 \ubd84\ud3ec)", ok1,
        "\ub9e4\uce6d\uc218 / \uacbd\uc6b0\uc758\uc218 / \uc774\ub860\ud655\ub960 / \ubaa8\uc758\ud655\ub960 \ub124 \uc5f4, \ub2e4\uc12f \ud589\uc73c\ub85c \ub2f4\uc73c\uc138\uc694. \uacbd\uc6b0\uc758\uc218\ub294 \uc704\uc5d0\uc11c\ubd80\ud130 9, 8, 6, 0, 1 \uc774\uace0 \ud569\uc774 24 \uc785\ub2c8\ub2e4. \uc774 \ub2e4\uc12f \uc22b\uc790\uac00 \ub9de\uc9c0 \uc54a\uc73c\uba74 \uc21c\uc5f4 24\uac00\uc9c0\ub97c \uc81c\ub300\ub85c \uac78\ub7ec\ub0b4\uc9c0 \ubabb\ud55c \uac83\uc785\ub2c8\ub2e4. \ubaa8\uc758\ud655\ub960\uc740 \uc774\ub860\ud655\ub960\uacfc 0.01 \uc548\uc5d0\uc11c \ub9de\uc544\uc57c \ud558\uace0, 3 \uc758 \uc790\ub9ac\ub294 \uc815\ud655\ud788 0 \uc774\uc5b4\uc57c \ud569\ub2c8\ub2e4.")

    ok2 <- safe({
      g <- as.data.frame(val("monty_fall"))
      need <- c("\uaddc\uce59", "\uc720\ud6a8\ud55c\ud310", "\ubc14\uafd4\uc11c\uc774\uae34\ube44\uc728")
      if (!got("monty_fall") || nrow(g) != 2 || !all(need %in% names(g))) FALSE else {
        n <- as.numeric(g[["\uc720\ud6a8\ud55c\ud310"]])
        p <- as.numeric(g[["\ubc14\uafd4\uc11c\uc774\uae34\ube44\uc728"]])
        abs(p[1] - 2/3) < 0.015 &&
        abs(p[2] - 1/2) < 0.015 &&
        abs(n[2] / n[1] - 2/3) < 0.02
      }
    })
    add("\ud544\uc218 \uacc4\uc0b0 2 : monty_fall (\uaddc\uce59\uc744 \ubc14\uafb8\uba74)", ok2,
        "\uaddc\uce59 / \uc720\ud6a8\ud55c\ud310 / \ubc14\uafd4\uc11c\uc774\uae34\ube44\uc728 \uc138 \uc5f4, \ub450 \ud589\uc73c\ub85c \ub2f4\uc73c\uc138\uc694. \uccab \uc904\uc774 2/3, \ub458\uc9f8 \uc904\uc774 1/2 \uadfc\ucc98\uc5ec\uc57c \ud569\ub2c8\ub2e4. \ub458\uc9f8 \uc904\uc774 2/3 \uac00 \ub098\uc624\uba74 \ubb34\ud6a8\uac00 \ub41c \ud310(monty == key)\uc744 \ube7c\uc9c0 \uc54a\uc740 \uac83\uc774\uace0, 1/3 \uc774 \ub098\uc624\uba74 \ubc14\uafb8\uc9c0 \uc54a\uc740 \ucabd\uc744 \uc13c \uac83\uc785\ub2c8\ub2e4. \uc720\ud6a8\ud55c \ud310\uc740 \uc804\uccb4\uc758 2/3 \uadfc\ucc98\uc785\ub2c8\ub2e4.")

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
