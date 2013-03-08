module Breadcrumbs
	where
import Import
import PostGenerated ()
import ToAppMessage

instance YesodBreadcrumbs App where
--dummy
	breadcrumb (StaticR _) = return("Static", Nothing)
	breadcrumb (AuthR _) = return("Auth", Nothing)
	breadcrumb (FaviconR) = return ("FaviconR",Nothing)
	breadcrumb (RobotsR) = return ("RobotsR",Nothing)
	breadcrumb (UserR _) = return ("",Nothing)
	breadcrumb (BeamerSlidePrivateR _) = return ("",Nothing)
	breadcrumb (BeamerSlidePublicR _) = return ("",Nothing)
	breadcrumb VocabtrainMobileVeecheckR = return ("",Nothing)

-- SOAP
	breadcrumb (VocabtrainTranslationR _) = return ("",Nothing)

-- Mobile
	breadcrumb (VocabtrainMobileBooksR) = return ("",Nothing)
	breadcrumb (VocabtrainMobileDownloadR) = return ("",Nothing)
	breadcrumb (VocabtrainMobileAuthTokenR) = return ("",Nothing)
	breadcrumb (VocabtrainMobileDeltaR) = return ("",Nothing)
	breadcrumb (VocabtrainMobileFilingUploadR) = return ("",Nothing)
	breadcrumb VocabtrainMobileFilingDownloadR = return ("",Nothing)
--JSON
	breadcrumb (VocabtrainCardQueryJR _ _) = return ("",Nothing)

	breadcrumb (VocabtrainCardQueryR language searchPhrase) = getMessageRender >>= \msg -> return(msg $ MsgBreadcrumbCardQuery (msg $ toAppMessage language) searchPhrase, Just VocabtrainR)
	breadcrumb (VocabtrainCardSearchR) = getMessageRender >>= \msg -> return(msg $ MsgBreadcrumbCardSearch, Just VocabtrainR)
	breadcrumb (VocabtrainOrphanedCardsR) = getMessageRender >>= \msg -> return(msg $ MsgBreadcrumbOrphanedCards, Just VocabtrainR)
	breadcrumb (VocabtrainNotTranslatedCardsR) = getMessageRender >>= \msg -> return(msg $ MsgBreadcrumbNotTranslatedCards, Just VocabtrainR)
	breadcrumb (UserUpdateR userId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbUserUpdate . fromRightText . fromPersistValue . unKey $ userId, Just VocabtrainR)

--root
	breadcrumb HomeR = return("Start", Nothing)
--dominik kurse
	breadcrumb DKHomeR = return("Kurshomepage", Just HomeR)
	breadcrumb DKProjectHomeR = return("Projekthomepage", Just HomeR)
	breadcrumb QtDescR = return("Qt Kurs", Just DKHomeR)
	breadcrumb QtDossierR = return("Qt Unterlagen", Just QtDescR)
	breadcrumb QtProjectListR = return("Qt Projekte", Just QtDescR)
	breadcrumb QtGalleryR = return("Qt Galerie", Just QtDescR)
	breadcrumb QtOpenGLR = return("Qt OpenGL", Just QtDescR)
	breadcrumb OpenGLR = return("OpenGL", Just DKHomeR)
	breadcrumb JavaDescR = return("Java Kurs", Just DKHomeR)
	breadcrumb JavaDossierR = return("Java Unterlagen", Just JavaDescR)
	breadcrumb JavaProjectListR = return("Java Projekte", Just JavaDescR)
	breadcrumb (JavaProjectR _) = return("", Just JavaProjectListR) 
--dominik projects
	breadcrumb ProjectAnnualR = return("Annual", Just DKProjectHomeR)
	breadcrumb ProjectFritzContactR = return("FritzContact", Just DKProjectHomeR)
	breadcrumb TatoebaAppR = return("例えば App", Just DKProjectHomeR)
	breadcrumb TatoebaWebServiceR = return("例えば Webservice", Just DKProjectHomeR)
	breadcrumb BeamerSlidesR = return("Slides", Just DKProjectHomeR)
--tatoeba
	breadcrumb TatoebaLanguagesR = getMessageRender >>= \msg -> return(msg $ MsgBreadcrumbTatoebaLanguages, Just TatoebaWebServiceR)
	breadcrumb (TatoebaQueryR language searchPhrase) = getMessageRender >>= \msg -> return(msg $ MsgBreadcrumbTatoebaQuery (msg $ toAppMessage language) searchPhrase,  Just TatoebaWebServiceR)
	breadcrumb (TatoebaQueryLanguageR language) = getMessageRender >>= \msg -> return(msg $ MsgBreadcrumbTatoebaQueryLanguage (msg $ toAppMessage language), Just TatoebaWebServiceR)
	breadcrumb TatoebaQueryRandomR = getMessageRender >>= \msg -> return(msg $ MsgBreadcrumbTatoebaQueryRandom, Just TatoebaWebServiceR)

--vocabtrain
	breadcrumb (VocabtrainBookLogR bookId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbBookLog . fromRightText . fromPersistValue . unKey $ bookId, Just VocabtrainR)
	breadcrumb (VocabtrainChapterLogR chapterId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbChapterLog . fromRightText . fromPersistValue . unKey $ chapterId, Just (VocabtrainChapterR chapterId))
	breadcrumb (VocabtrainTranslationLogR translationId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbTranslationLog . fromRightText . fromPersistValue . unKey $ translationId, Just VocabtrainR)
	breadcrumb (VocabtrainCardLogR cardId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCardLog . fromRightText . fromPersistValue . unKey $ cardId, Just (VocabtrainCardR cardId))

	breadcrumb (VocabtrainCardDuplicateSearchR cardId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCardDuplicateSearch . fromRightText . fromPersistValue . unKey $ cardId, Just (VocabtrainCardR cardId))
	breadcrumb (VocabtrainCardReplaceDuplicateR dupCardId origCardId) = getMessageRender >>= \msg ->
		return(msg $ MsgBreadcrumbCardReplaceDuplicate (fromRightText . fromPersistValue . unKey $ dupCardId) (fromRightText . fromPersistValue . unKey $ origCardId), Just $ VocabtrainCardDuplicateSearchR dupCardId)
	breadcrumb VocabtrainBookInsertR = getMessageRender >>= \msg -> return(msg $ MsgBreadcrumbBookInsert, Just VocabtrainR)


	breadcrumb VocabtrainR = return("Vocabtrain", Just HomeR)
	breadcrumb (VocabtrainBookUpdateR bookId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbBookUpdate . fromRightText . fromPersistValue . unKey $ bookId, Just VocabtrainR)
	breadcrumb (VocabtrainBookDeleteR bookId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbBookDelete . fromRightText . fromPersistValue . unKey $ bookId, Just VocabtrainR)
	breadcrumb (VocabtrainMissingTranslationForCardsR bookId language) = getMessageRender >>= \msg -> return(msg $ MsgBreadcrumbMissingTranslationForCards (fromRightText . fromPersistValue . unKey $ bookId) (msg $ toAppMessage language), Just VocabtrainR)

	breadcrumb (VocabtrainChapterR chapterId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbChapter . fromRightText . fromPersistValue . unKey $ chapterId, Just VocabtrainR)
	breadcrumb (VocabtrainChapterInsertR bookId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbChapterInsert . fromRightText . fromPersistValue . unKey $ bookId, Just VocabtrainR)
	breadcrumb (VocabtrainChapterUpdateR chapterId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbChapterUpdate . fromRightText . fromPersistValue . unKey $ chapterId, Just (VocabtrainChapterR chapterId))
	breadcrumb (VocabtrainChapterDeleteR chapterId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbChapterDelete . fromRightText . fromPersistValue . unKey $ chapterId, Just (VocabtrainChapterR chapterId))

	breadcrumb (VocabtrainTranslationInsertR cardId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbTranslationInsert . fromRightText . fromPersistValue . unKey $ cardId, Just (VocabtrainCardR cardId))
	breadcrumb (VocabtrainTranslationUpdateR translationId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbTranslationUpdate . fromRightText . fromPersistValue . unKey $ translationId, Just VocabtrainR)
	breadcrumb (VocabtrainTranslationDeleteR translationId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbTranslationDelete . fromRightText . fromPersistValue . unKey $ translationId, Just VocabtrainR)

	breadcrumb (VocabtrainCardChapterAddR chapterId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCardChapterAdd . fromRightText . fromPersistValue . unKey $ chapterId, Just (VocabtrainChapterR chapterId))
	breadcrumb (VocabtrainCardChapterInsertR cardId chapterId) = getMessageRender >>= \msg -> return(msg $ MsgBreadcrumbCardChapterInsert (fromRightText . fromPersistValue . unKey $ cardId) (fromRightText . fromPersistValue . unKey $ chapterId), Just (VocabtrainChapterR chapterId))
	breadcrumb (VocabtrainCardChaptersDeleteR cardId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCardChaptersDelete . fromRightText . fromPersistValue . unKey $ cardId, Just (VocabtrainCardR cardId))

	breadcrumb (VocabtrainCardR cardId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCard . fromRightText . fromPersistValue . unKey $ cardId, Just VocabtrainR)
	breadcrumb (VocabtrainCardInsertR chapterId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCardInsert . fromRightText . fromPersistValue . unKey $ chapterId, Just (VocabtrainChapterR chapterId))
	breadcrumb (VocabtrainCardUpdateR cardId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCardUpdate . fromRightText . fromPersistValue . unKey $ cardId, Just (VocabtrainCardR cardId))
	breadcrumb (VocabtrainCardDeleteR cardId) = getMessageRender >>= \msg -> return(msg . MsgBreadcrumbCardDelete . fromRightText . fromPersistValue . unKey $ cardId, Just (VocabtrainCardR cardId))


--	breadcrumb _ = return("", Nothing)

