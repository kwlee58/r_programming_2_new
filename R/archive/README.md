# R/archive

여기 있는 파일들은 **버려진 것이 아니라 물러난 것**입니다. 지금 쓰는 자료가
어떤 시행착오를 거쳐 나왔는지 남겨 두려고 옮겼습니다. 언제든 되돌릴 수 있습니다.

```bash
git mv R/archive/<파일> R/<파일>
```

## 미나르 지도 (2025-12 ~ )

나폴레옹의 1812년 러시아 원정 지도를 `HistData` 자료로 다시 그리고
애니메이션으로 만드는 작업의 중간 단계들입니다.

| 파일 | 무엇 | 왜 물러났나 |
|---|---|---|
| `Minard.Rmd` | 정적 ggplot, 초기본 | `Minard_HistData.Rmd` 로 정리됨 |
| `Minard_HistData.R` | 위의 R 스크립트판 | Rmd 판과 중복 |
| `Minard_GPT.Rmd` | 정적, 영문 라벨 | 애니메이션 계열로 넘어감 |
| `Minard_GPT_v2.Rmd` | 첫 애니메이션 시도 | `transition_reveal(along = survivors)` — 시간이 아닌 변수를 시간축으로 씀 |
| `Minard_GPT_v4.Rmd` | 날짜 기반, 영문 | v5·v6 으로 이어짐 |
| `Minard_GPT_v5.Rmd` | v4 + lubridate 정리 | v6 으로 이어짐 |
| `Minard_GPT_v6.Rmd` | v5 + 글자 크기 조정 | 한글판(Gemini 계열)으로 넘어감 |
| `Minars_Gemini_v2.Rmd` | 한글판 (파일명 오타 `Minars`) | `Minard_Gemini.Rmd` 와 거의 동일 |

### 지금 쓰는 것

- `R/Minard_v2.Rmd` — 정리본. 아래 다섯 가지를 고쳤습니다.
  1. `transition_reveal()` 의 자막 변수는 `{frame_time}` 이 아니라 `{frame_along}`
  2. `transition_reveal(date_num)` → `transition_reveal(date)` (숫자가 아니라 날짜를 넘겨야 자막에 날짜가 찍힘)
  3. 프레임 렌더링용 한글 폰트 지정 (`base_family`) + `animate(device = "ragg_png")`
  4. `geom_path` 의 `size` → `linewidth` (ggplot2 3.4 이후)
  5. `geom_text` 의 `na_rm` → `na.rm` (오타)
- `R/Minard_Gemini.Rmd` — 정리본의 직계 원본. 비교용으로 R/ 에 남겨 둠
- `R/Minard_HistData.Rmd` — 정적 그림의 기준본

### 남은 흔적 하나

`R/gganim_plot0001.png` ~ `0100.png` 는 `Minard_Gemini.Rmd` 의 렌더가 150 프레임
중 100번째에서 끊기며 남은 중간 산출물입니다. `.gitignore` 에 넣어 추적에서
뺐습니다. 디스크에서 지우셔도 무방합니다.
