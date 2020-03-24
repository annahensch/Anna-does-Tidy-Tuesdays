Traumatic Brain Injury
================
Anna Henschel
2020-03-24

# Submission for \#TidyTuesday week 13 “Traumatic Brain Injury”

``` r
library(hrbrthemes)
library(ggsci)
library(extrafont)
library(patchwork)
library(tidyverse)
```

# Load the data

Plot:

``` r
# ggplot(aes(x = domgross, y = intgross, colour = as_factor(decade_code))) + 
#   geom_jitter(alpha = .7, size = 4) +
#   scale_x_continuous(labels = scales::dollar_format(prefix="$")) +
#   scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
#   hrbrthemes::theme_modern_rc() +
#   scale_color_startrek() +
#   theme(axis.text.x = element_text(size =16), 
#         axis.text.y = element_text(size =16)) +
#   labs(x = '', y = '',
#        colour = "By decade:", 
#          title = 'Traumatic Brain Injury',
#        caption = 'Data by CDC • #braininjuryawarenessmonth • @AnnaHenschel') 
```
