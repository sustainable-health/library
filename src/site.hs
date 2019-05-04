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
    
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "docs/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "about.md" $ do
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
                >>= loadAndApplyTemplate "templates/page.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "topics/*/*/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler 
            >>= loadAndApplyTemplate "templates/angles-right-column.html" (topicCtx tags <> mainCtx tags "topics")
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls    

    match "topics/*/*.md" $ do
        route $ setExtension "html"
        compile $ do
            content <- pandocCompiler >>= saveSnapshot "content"
            let md = itemIdentifier content
                topicDir = fst $ splitFileName $ toFilePath md
            sf <- getMetadataField' md "angles-folder" 
            angles  <- loadAll $ fromGlob $ topicDir <> sf <> "/*"
            let anglesCtx =
                    listField "angles" (topicCtx tags) (return angles) <>
                    defaultContext

            loadAndApplyTemplate "templates/topic-right-column.html" (topicCtx tags <> anglesCtx <> mainCtx tags "topics/*/*") content
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls    

    paginate <- buildPaginateWith topicsGrouper "topics/*/*" topicsPageId

    paginateRules paginate $ \page pattern -> do
        route idRoute
        compile $ do
            topics <- recentFirst =<< loadAllSnapshots pattern "content"
            let indexCtx =
                    constField "title" (if page == 1 then "Reception desk"
                                                     else "More topics, page " ++ show page) <>
                    listField "topics" (previewCtx tags) (return topics) <>
                    paginateContextPlus paginate page <>
                    mainCtx tags "topics/*/*"

            makeItem ""
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/topics-preview-list.html" indexCtx
                >>= loadAndApplyTemplate "templates/page-right-column.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


stripPages = gsubRoute "pages/" $ const ""

mainCtx :: Tags -> Pattern -> Context String
mainCtx tags pattern =
    let angles = angleItems "topics/*/*" >>= fmap (take 5) . recentFirst in
         listField "angles" (previewCtx tags) angles <> 
      tagCloudField "tagCloud" 75 200 tags <>
      defaultContext

topicCtx :: Tags -> Context String
topicCtx tags =
    dateField "date" "%B %e, %Y" <>
    tagsField "tags" tags <>
    defaultContext

previewCtx :: Tags -> Context String
previewCtx tags = teaserField "preview" "content" <> topicCtx tags

angleItems :: Pattern ->  Compiler [Item String]
angleItems pattern = do
    identifiers <- getMatches "topics/*/*"
    return [Item identifier "" | identifier <- identifiers]

topicsGrouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
topicsGrouper = liftM (paginateEvery 10) . sortRecentFirst

topicsPageId :: PageNumber -> Identifier
topicsPageId n = fromFilePath $ if (n == 1) then "index.html" else show n ++ "/index.html"

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
