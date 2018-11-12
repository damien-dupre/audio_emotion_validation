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
  #dplyr::mutate_at(vars("emotion_valence_c","emotion_arousal_c","rec_valence","rec_arousal"), funs(as.factor(as.character(.)))) %>%
  as.data.frame()

test$emotion_valence_c <- as.factor(as.character(test$emotion_valence_c))
test$emotion_arousal_c <- as.factor(as.character(test$emotion_arousal_c))
test$rec_valence <- as.factor(as.character(test$rec_valence))
test$rec_arousal <- as.factor(as.character(test$rec_arousal))

str(test)
valence_confusion_matrix <- caret::confusionMatrix(test$emotion_valence_c, test$rec_valence)
arousal_confusion_matrix <- caret::confusionMatrix(test$emotion_arousal_c, test$rec_arousal)

valence_confusion_matrix


ggplot(test, aes(emotion_valence, segments.emotion.dimensions.valence)) +
  geom_jitter() +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)

ggplot(test, aes(emotion_arousal, segments.emotion.dimensions.activation)) +
  geom_jitter() +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)

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
