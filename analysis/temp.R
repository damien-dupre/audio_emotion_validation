test <- data.table::fread(here::here("./data","audeering_gemep.csv")) %>%
  dplyr::filter(emotion %in% c("joy", "amu","pri", "ang","fea", "des","ple", "rel","int", "irr","anx", "sad")) %>%
  dplyr::select(name_audio, ppt, emotion,contains("emotion")) %>%
  dplyr::mutate(emotion_valence = case_when(
    emotion %in% c("joy","amu","pri","ple","rel","int") ~ "positive",
    emotion %in% c("ang","fea","des","irr","anx","sad") ~ "negative"
  )) %>%
  dplyr::mutate(emotion_valence_c = case_when(
    emotion %in% c("joy","amu","pri","ple","rel","int") ~ 1,
    emotion %in% c("ang","fea","des","irr","anx","sad") ~ 0
  )) %>%
  dplyr::mutate(emotion_arousal = case_when(
    emotion %in% c("joy", "amu","pri", "ang","fea", "des") ~ "high",
    emotion %in% c("ple", "rel","int", "irr","anx", "sad") ~ "low"
  )) %>%
  dplyr::mutate(emotion_arousal_c = case_when(
    emotion %in% c("joy", "amu","pri", "ang","fea", "des") ~ 1,
    emotion %in% c("ple", "rel","int", "irr","anx", "sad") ~ 0
  )) %>%
  dplyr::group_by(name_audio,emotion, emotion_valence, emotion_arousal) %>%
  dplyr::summarise_if(is.numeric,mean) %>%
  dplyr::mutate(rec_arousal = ifelse(segments.emotion.dimensions.activation>0, 1,0)) %>%
  dplyr::mutate(rec_valence = ifelse(segments.emotion.dimensions.valence>0, 1,0)) %>%
  as.data.frame()

test$emotion_valence_c <- as.factor(as.character(test$emotion_valence_c))
test$emotion_arousal_c <- as.factor(as.character(test$emotion_arousal_c))
test$rec_valence <- as.factor(as.character(test$rec_valence))
test$rec_arousal <- as.factor(as.character(test$rec_arousal))

valence_confusion_matrix <- caret::confusionMatrix(test$emotion_valence_c, test$rec_valence)
arousal_confusion_matrix <- caret::confusionMatrix(test$emotion_arousal_c, test$rec_arousal)

valence_confusion_matrix$overall["AccuracyLower"]



ggplot(test, aes(emotion_valence, segments.emotion.dimensions.valence)) +
  geom_jitter() +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)

ggplot(test, aes(emotion_arousal, segments.emotion.dimensions.activation)) +
  geom_jitter() +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)

##################################################################################



## data
# 10 actors expressing:
# 12 diffents emotions
# - High Elation (joy)
# - Amusement (amu)
# - Pride (pri)
# - Hot anger (ang)
# - Panic fear (fea)
# - Despair (des)
# - Low Pleasure (ple)
# - Relief (rel)
# - Interest (int)
# - Cold anger (irr)
# - Anxiety (anx)
# - Sadness (sad)

# 5 additional states
# - surprise (sur)
# - admiration (adm)
# - disgust (dis)
# - contempt (con)
# - tenderness (ten)



# Recognition of 23 affective states (/amu, /des, /ple, /rel, /sur, /adm, /ten)
# passion
# panic
# nervousness
# disgust* (dis)
# contentment* (con)
# affection
# fear* (fea)
# irritation* (irr)
# satisfaction
# frustration
# enthusiasm
# worry* (anx)
# boredom
# interest* (int)
# tension
# joy* (joy)
# depression
# stress
# pride* (pri)
# excitement
# sadness* (sad)
# anger* (ang)
# relaxation
# happiness

test <- data.table::fread(here::here("./data","audeering_gemep.csv")) %>%
  dplyr::filter(emotion %in% c("dis", "con","fea", "irr","anx", "int","joy", "pri","sad", "ang")) %>%
  dplyr::select(name_audio, ppt, emotion,contains("emotion")) #%>%
  dplyr::mutate(rec_dis = ifelse(segments.emotion.category_scores.disgust>0, 1,0)) %>%
  dplyr::mutate(rec_con = ifelse(segments.emotion.category_scores.contentment>0, 1,0)) %>%
  dplyr::mutate(rec_fea = ifelse(segments.emotion.category_scores.fear>0, 1,0)) %>%
  dplyr::mutate(rec_irr = ifelse(segments.emotion.category_scores.irritation>0, 1,0)) %>%
  dplyr::mutate(rec_anx = ifelse(segments.emotion.category_scores.worry>0, 1,0)) %>%
  dplyr::mutate(rec_int = ifelse(segments.emotion.category_scores.interest>0, 1,0)) %>%
  dplyr::mutate(rec_joy = ifelse(segments.emotion.category_scores.joy>0, 1,0)) %>%
  dplyr::mutate(rec_pri = ifelse(segments.emotion.category_scores.pride>0, 1,0)) %>%
  dplyr::mutate(rec_sad = ifelse(segments.emotion.category_scores.sadness>0, 1,0)) %>%
  dplyr::mutate(rec_ang = ifelse(segments.emotion.category_scores.anger>0, 1,0))
  
test <- data.table::fread(here::here("./data","audeering_gemep.csv"))%>%
  dplyr::filter(emotion %in% c("dis", "con","fea", "irr","anx", "int","joy", "pri","sad", "ang")) %>%
  dplyr::mutate(emotion_c = case_when(
    emotion == "dis" ~ "disgust",
    emotion == "con" ~ "contentment",
    emotion == "fea" ~ "fear",
    emotion == "irr" ~ "irritation",
    emotion == "anx" ~ "worry",
    emotion == "int" ~ "interest",
    emotion == "joy" ~ "joy",
    emotion == "pri" ~ "pride",
    emotion == "sad" ~ "sadness",
    emotion == "ang" ~ "anger"
  )) %>%
  dplyr::mutate(match_emotion = ifelse(emotion_c == segments.emotion.category.name, 1, 0))

scales::percent(sum(test$match_emotion)/nrow(test))

