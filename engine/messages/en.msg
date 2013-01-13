ChapterCreated title@Text: Your new chapter, #{title}, has been created.
ChapterUpdated title@Text: The chapter #{title}, has been successfully updated.
ChapterDeleted title@Text: The chapter #{title}, has been deleted.
ChapterDeleteHeader title@Text: Do you really want to delete the chapter #{title}?
ChapterUpdateHeader title@Text: Update chapter #{title}
ChapterPleaseCorrectEntry: Your submitted entry had some errors, please correct and try again.
ChapterNotFound id@Int: The chapter ##{id} could not be found.
ChapterDelete: Delete chapter
ChapterUpdate: Update chapter
ChapterNew: Create a new chapter
ChapterNewHeader: New chapter
ChapterHeader title@Text: Chapter #{title}
ChapterListHeader: Chapters

Books: Books
BookCreated title@Text: Your new book, #{title}, has been created.
BookUpdated title@Text: The book #{title}, has been successfully updated.
BookDeleted title@Text: The book #{title}, has been deleted.
BookListHeader: Book list
BookDeleteHeader title@Text: Do you really want to delete the book #{title}?
BookUpdateHeader title@Text: Update book #{title}
BookPleaseCorrectEntry: Your submitted entry had some errors, please correct and try again.
BookNotFound id@Int: The book ##{id} could not be found.
BookLog: Book Log
BookDelete: Delete Book
BookEdit: Edit Book
BookUpdate: Update Book
BookNew: Create a new book
BookNewHeader: New book

BreadcrumbBookUpdate bookId@Text: Update Book ##{bookId}
BreadcrumbBookDelete bookId@Text: Delete Book ##{bookId}
BreadcrumbChapter chapterId@Text: Chapter ##{chapterId}
BreadcrumbChapterInsert bookId@Text: Add Chapter to Book ##{bookId}
BreadcrumbChapterUpdate chapterId@Text: Update Chapter ##{chapterId}
BreadcrumbChapterDelete chapterId@Text: Delete Chapter ##{chapterId}
BreadcrumbTranslationInsert cardId@Text: Add Translation to Card ##{cardId}
BreadcrumbTranslationUpdate translationId@Text: Update Translation ##{translationId}
BreadcrumbTranslationDelete translationId@Text: Delete Translation ##{translationId}
BreadcrumbCardChapterAdd chapterId@Text: Add Card to Chapter ##{chapterId}
BreadcrumbCardChapterInsert cardId@Text chapterId@Text: Connect Chapter ##{chapterId} and Card ##{cardId}
BreadcrumbCardChaptersDelete cardId@Text: Unlink Card ##{cardId} from Chapters
BreadcrumbCard cardId@Text: Card ##{cardId}
BreadcrumbCardInsert cardId@Text: Insert Card ##{cardId}
BreadcrumbCardUpdate cardId@Text: Update Card ##{cardId}
BreadcrumbCardDelete cardId@Text: Delete Card ##{cardId}

TranslationCreated title@Text: Your new translation, #{title}, has been created.
TranslationUpdated title@Text: The translation #{title}, has been successfully updated.
TranslationDeleted title@Text: The translation #{title}, has been deleted.
TranslationDeleteHeader title@Text: Do you really want to delete the translation #{title}?
TranslationUpdateHeader title@Text: Update translation #{title}
TranslationPleaseCorrectEntry: Your submitted entry had some errors, please correct and try again.
TranslationNotFound id@Int: The translation ##{id} could not be found.
TranslationDelete: Delete translation
TranslationEdit: Edit translation
TranslationUpdate: Update translation
TranslationNew: Create a new translation
TranslationNewHeader: New translation

CardCreated title@Text: Your new card, #{title}, has been created.
CardUpdated title@Text: The card #{title}, has been successfully updated.
CardDeleted title@Text: The card #{title}, has been deleted.
CardDeleteHeader title@Text: Do you really want to delete the card #{title}?
CardUpdateHeader title@Text: Update card #{title}
CardPleaseCorrectEntry: Your submitted entry had some errors, please correct and try again.
CardNotFound id@Int: The card ##{id} could not be found.
CardDelete: Delete card
CardEdit: Edit card
CardUpdate: Update card
CardNew: Create a new card
CardNewHeader: New card

CardSearchHeader title@Text: Search for cards containing `#{title}'
CardOrphanedHeader: Orphaned Cards
CardSearch: Search Card
CardChapterSearchHeader: Search cards to add to this chapter
FieldSearchCard: Search Card
CardFoundInTheFollowingChapters: Belongs to


Nobody: Anonymous
NobodyHeader: Anonymous Coward
UserHeader title@Text: User #{title}
UserUpdate: Update User
UserUpdated title@Text: User `#{title}` successfully updated.
NobodyUpdated: Anonym profile successfully updated;
UserUpdateHeader: Update User
FieldUserNick: Nickname
FieldUserPassword: Password
UserNickNotFound title@Text: Could not find any user with the nick `#{title}`
UserIdNotFound userId@UserId: Could not find the user with id ##{Text.pack $ show $ userId}
UserManipulationPermissionDenied: You cannot edit the profile of another user!
UserNickAlreadyForgiven: Sorry, but this nickname is already forgiven.
NotTrustedUser: You are not a trusted user.

BookLogHeader book@VocabBook: ChangeLog of book #{vocabBookName book}

TooltipVocabtrainBookLogR: Show Changelog of this book
TooltipVocabtrainCardShow: Show Card
CardChapterCreated: Successfully added card to chapter.
CardChapterInsert: Add Card
CardChapterAddHeader title@Text: Cards containing the keyword #{title}
CardChaptersDeleteHeader title@Text: Remove card ,#{title}, from the chapters
CardChaptersDelete: Remove Links
CardChapterAdded title@Text: As the card #{title} already existed, we've merely added it to this chapter.
CardUpdateToDuplicate: A card with this description already exists, so you cannot change this card to a duplicate.
CardChaptersDeleted title@Text: Successfully card #{title} from selected chapters removed.

TooltipVocabtrainTranslationUpdateR: Update Translation
TooltipVocabtrainTranslationDeleteR: Delete Translation
TooltipTatoebaQueryR: Find example sentences
TooltipVocabtrainCardUpdate: Update Card
TooltipVocabtrainCardDelete: Delete Card
TooltipVocabtrainBookUpdateR: Update Book
TooltipVocabtrainBookDeleteR: Delete Book
TooltipVocabtrainChapterUpdateR: Update Chapter
TooltipVocabtrainChapterDeleteR: Delete Chapter

BookLastChanged time@UTCTime: The last change on this book was made on #{show time}

CardTypeNone:
CardTypeTransitive: trans.
CardTypeIntransitive: intr.
CardTypeReflexive: refl.
CardTypeGodanDoushi: 五段動詞
CardTypeIchidanDoushi: 一段動詞
CardTypeIrregularDoushi: irreg. 動詞
CardTypeComparative: comp.
CardTypeSuperlative: super
CardTypeNaAdjective: ナ
CardTypeIAdjective: イ
CardTypePreposition: prep.
CardTypePostposition: postp.
CardTypeParticle: part.
CardTypeFeminine: fem.
CardTypeMasculine: mas.
CardTypeNeuter: neu.
CardTypeFemininePlural: fem.pl.
CardTypeMasculinePlural: mas.pl.
CardTypeNeuterPlural: neu.pl.
CardTypeUnknown: Unknown
CardTypeVerb: Verb
CardTypeAdjective: Adjective
CardTypeAdverb: Adverb
CardTypeAdposition: Adposition
CardTypeConjugation: Conjugation
CardTypeAbbreviation: Abbreviation
CardTypeSaw: Saw
CardTypeNoun: Noun


CardHeader title@Text: Examine Card `#{title}'

VocabManipulationPermissionDenied: You have to login in order to make changes.

FieldLanguage: Language
FieldName: Name
FieldVolume: Volume
FieldChapters: Chapters
FieldComment: Comment
FieldTranslation: Translation

FieldScript: Script
FieldScriptComment: Script Comment
FieldSpeech: Speech
FieldSpeechComment: Speech Comment

FieldCardType: Type
FieldCardTypePrimary: Primary Type
FieldCardTypeSecondary: Secondary Type
FieldCardTypeTertiary: Tertiary Type

Lang_eng: English
Lang_epo: Esperanto
Lang_fra: French
Lang_deu: German
Lang_jpn: Japanese
Lang_spa: Spanish
Lang_tur: Turkish
Lang_por: Portuguese
Lang_ita: Italian
Lang_heb: Hebrew
Lang_rus: Russian
Lang_pol: Polish
Lang_cmn: Chinese
Lang_ber: Berber
Lang_nld: Dutch
Lang_hun: Hungarian
Lang_ukr: Ukrainian
Lang_nds: Low Saxon
Lang_pes: Persian
Lang_ara: Arabic
Lang_dan: Danish
Lang_isl: Icelandic
Lang_lat: Latin
Lang_swe: Swedish
Lang_jbo: Lojban
Lang_fin: Finnish
Lang_srp: Serbian
Lang_vie: Vietnamese
Lang_lit: Lithuanian
Lang_tlh: Klingon
Lang_bul: Bulgarian
Lang_tgl: Tagalog
Lang_hin: Hindi
Lang_uig: Uyghur
Lang_cat: Catalan
Lang_ces: Czech
Lang_wuu: Shanghainese
Lang_glg: Galician
Lang_ina: Interlingua
Lang_ell: Modern Greek
Lang_ido: Ido
Lang_nob: Norwegian (Bokmål)
Lang_bel: Belarusian
Lang_ron: Romanian
Lang_yue: Cantonese
Lang_mar: Marathi
Lang_oci: Occitan
Lang_hrv: Croatian
Lang_ind: Indonesian
Lang_tat: Tatar
Lang_eus: Basque
Lang_lzh: Literary Chinese
Lang_kaz: Kazakh
Lang_toki: Toki Pona
Lang_urd: Urdu
Lang_kor: Korean
Lang_orv: Old East Slavic
Lang_afr: Afrikaans
Lang_zsm: Malay
Lang_avk: Kotava
Lang_slk: Slovak
Lang_xal: Kalmyk
Lang_mal: Malayalam
Lang_non: Norwegian (Nynorsk)
Lang_est: Estonian
Lang_arz: Egyptian Arabic
Lang_lvs: Latvian
Lang_vol: Volapük
Lang_bre: Breton
Lang_yid: Yiddish
Lang_gla: Scottish Gaelic
Lang_mon: Mongolian
Lang_hsb: Upper Sorbian
Lang_dsb: Lower Sorbian
Lang_gle: Irish
Lang_kur: Kurdish
Lang_cym: Welsh
Lang_que: Quechua
Lang_ile: Interlingue
Lang_kat: Georgian
Lang_sqi: Albanian
Lang_uzb: Uzbek
Lang_tha: Thai
Lang_oss: Ossetian
Lang_swh: Swahili
Lang_ang: Old English
Lang_nov: Novial
Lang_qya: Quenya
Lang_bos: Bosnian
Lang_xho: Xhosa
Lang_fry: Frisian
Lang_slv: Slovenian
Lang_lld: Ladin
Lang_tpi: Tok Pisin
Lang_lad: Ladino
Lang_tel: Telegu
Lang_cycl: CycL
Lang_ast: Asturian
Lang_ben: Bengali
Lang_acm: Iraqi Arabic
Lang_tpw: Old Tupi
Lang_pms: Piemontese
Lang_hye: Armenian
Lang_cha: Chamorro
Lang_tgk: Tajik
Lang_ewe: Ewe
Lang_mlg: Malagasy
Lang_pnb: Punjabi
Lang_grn: Guarani
Lang_scn: Sicilian
Lang_fao: Faroese
Lang_mri: Maori
Lang_und: Undefined
Lang_ain: Ainu
Lang_san: Sanskrit
Lang_ksh: Kölsch
Lang_nan: Teochew
Lang_roh: Romansh
Lang_sjn: Sindarin 

VocabWelcome: Welcome to Vocabtrain
VocabIntroduction: Upset with the current supply of vocabulary trainers with insufficient control or bad vocabulary?  Then try Vocabtrain, a new approach of community-driven vocabulary-creation with optimization for asian languages (CJK).  Instead of writing for each training book all vocabulary by hand (which is error-prone), you can simply import already inserted vocabulary from other books (even with already inserted translations). Use the book-list on the left for navigation.

Hello: Hello
NotAnAdmin: You must be an administrator to access this page.

WelcomeHomepage: Welcome to the homepage
SeeArchive: See the archive

NoEntries: There are no entries in the blog
LoginToPost: Admins can login to post
NewEntry: Post to blog
NewEntryTitle: Title
NewEntryContent: Content

FieldManipulationType: Type
FieldTimestamp: Timestamp
FieldContent: Content

UserManipInsert: Inserted
UserManipDelete: Deleted
UserManipUpdate: Updated

PleaseCorrectEntry: Your submitted entry had some errors, please correct and try again.
EntryCreated title@Text: Your new blog post, #{title}, has been created

EntryTitle title@Text: Blog post: #{title}
CommentsHeading: Comments
NoComments: There are no comments
AddCommentHeading: Add a Comment
LoginToComment: You must be logged in to comment
AddCommentButton: Add comment

CommentName: Your display name
CommentText: Comment
CommentAdded: Your comment has been added
PleaseCorrectComment: Your submitted comment had some errors, please correct and try again.

HomepageTitle: Yesod Blog Demo
BlogArchiveTitle: Blog Archive


