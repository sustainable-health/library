--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Control.Monad       (liftM)
import           Data.List           (partition)
import qualified Data.Map            as M
import           Data.Monoid         ((<>))
import           Hakyll
import           System.Environment  (getArgs, withArgs)
import           System.FilePath.Posix (splitFileName)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    let
        topicMdPattern = "topics/*/*.md"
        topicPattern = "topics/*/*"
        anglePattern = "topics/*/*/*"
        angleMdPattern = "topics/*/*/*.md"
    
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "docs/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "videos/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.md", "contact.markdown"]) $ do
        route  $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls 

    match "404.md" $ do
        route  $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/404-page.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls 

    tags <- buildTags "topics/**" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Results for " ++ tag
        route idRoute
        compile $ do
            topics <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                      listField "topics" (topicCtx tags) (return topics) <>
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/topics.html" ctx
                >>= loadAndApplyTemplate "templates/tags.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    
    match angleMdPattern $ do
        route $ setExtension "html"
        compile $ do
            content <- pandocCompiler
            let
                ownId = itemIdentifier content
                siblings = siblingAnglesCtx ownId
            loadAndApplyTemplate "templates/resources.html" 
                (siblings <> topicCtx tags <> tagCtx tags)
                content
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls    

    match topicMdPattern $ do
        route $ setExtension "html"
        compile $ do
            content <- pandocCompiler >>= saveSnapshot "content"
            let md = itemIdentifier content
                topicDir = fst $ splitFileName $ toFilePath md
            -- sf <- getMetadataField' md "angles-folder" 
            angles  <- loadAll $ fromGlob $ topicDir <> "angles/*.md"
            let anglesCtx =
                    listField "angles" (topicCtx tags) (return angles) <>
                    defaultContext

            loadAndApplyTemplate "templates/topic-expanded.html" (topicCtx tags <> anglesCtx <> tagCtx tags) content
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls   

    -- match "index.html" $ do
    --     route idRoute
    --     compile $ do
    --         topics <- loadAll "topics/*.md"
    --         -- recentFirst =<< loadAll "posts/*"
    --         let indexCtx =
    --                 listField "topics" (topicCtx tags) (return topics) `mappend` 
    --                 constField "title" "Reception Desk"  `mappend`
    --                 (tagCtx tags)  <>          
    --                 defaultContext

    --         getResourceBody
    --             >>= applyAsTemplate indexCtx
    --             >>= loadAndApplyTemplate "templates/page-right-column.html" indexCtx
    --             >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --             >>= relativizeUrls

    paginateTopics <- buildPaginateWith topicsGrouper topicPattern topicsPageId

    paginateRules paginateTopics $ \page pattern -> do
        route idRoute
        compile $ do
            topics <- recentFirst =<< loadAllSnapshots pattern "content"
            let indexCtx =
                    constField "title" (if page == 1 then "Reception desk"
                                                     else "More topics, page " ++ show page) <>
                    listField "topics" (previewCtx tags) (return topics) <>
                    paginateContextPlus paginateTopics page <>
                    tagCtx tags 

            makeItem ""
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/topics-preview-list.html" indexCtx
                >>= loadAndApplyTemplate "templates/page-right-column.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


stripPages = gsubRoute "pages/" $ const ""

tagCtx :: Tags -> Context String
tagCtx tags =
            tagCloudField "tagCloud" 75 200 tags <>
            defaultContext

siblingAnglesCtx :: Identifier -> Context String
siblingAnglesCtx angleId =
    listField "angles" defaultContext items <> defaultContext
    where
        anglesDir = fst $ splitFileName $ toFilePath angleId
        ids = getMatches $ fromGlob $ anglesDir <> "*"
        idsExceptOwn = filter (/= angleId) <$> ids
        f id = Item id ""
        items = (fmap . fmap) f idsExceptOwn

topicCtx :: Tags -> Context String
topicCtx tags =
  --  dateField "date" "%B %e, %Y" <>
    tagsField "tags" tags <>
    defaultContext

previewCtx :: Tags -> Context String
previewCtx tags = teaserField "preview" "content" <> topicCtx tags

angleItems :: Pattern -> Compiler [Item String]
angleItems pattern = do
    identifiers <- getMatches pattern
    return [Item identifier "" | identifier <- identifiers]

topicsGrouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
topicsGrouper = liftM (paginateEvery 10) . sortRecentFirst


topicsPageId :: PageNumber -> Identifier
topicsPageId n = fromFilePath $ if (n == 1) then "index.html" else show n ++ "/index.html"

resourcesGrouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
resourcesGrouper = liftM (paginateEvery 10) . sortRecentFirst

paginateContextPlus :: Paginate -> PageNumber -> Context a
paginateContextPlus pag currentPage = paginateContext pag currentPage <> mconcat
    [ listField "pagesBefore" linkCtx $ wrapPages pagesBefore
    , listField "pagesAfter"  linkCtx $ wrapPages pagesAfter
    ]
    where
        linkCtx = field "pageNum" (return . fst . itemBody) <>
                  field "pageUrl" (return . snd . itemBody)
        lastPage = M.size . paginateMap $ pag
        pageInfo n = (n, paginateMakeId pag n)

        pages = [pageInfo n | n <- [1..lastPage], n /= currentPage]
        (pagesBefore, pagesAfter) = span ((< currentPage) . fst) pages

        wrapPages = sequence . map makeInfoItem

        makeInfoItem (n, i) = getRoute i >>= \mbR -> case mbR of
            Just r  -> makeItem (show n, toUrl r)
            Nothing -> fail $ "No URL for page: " ++ show n
