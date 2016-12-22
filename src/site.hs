--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.List (partition)
import qualified Data.Map as M
import Data.Monoid ((<>))
import System.Environment (getArgs, withArgs)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = checkArgs <$> getArgs >>=
        \(postsPattern, conf, args) -> withArgs args $ hakyllWith conf $ do

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["pages/about.md", "pages/404.md"]) $ do
        route   $ stripPages `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags postsPattern (fromCapture "tags/*.html")

    paginate <- buildPaginateWith postsGrouper postsPattern makePageId

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route stripPages
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                      listField "posts" (postCtx tags) (return posts) <>
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/page.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match postsPattern $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post-with-comment.html" defaultContext
            >>= loadAndApplyTemplate "templates/post-right-column.html" (postCtx tags <> mainCtx tags postsPattern)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = (postCtx tags) <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots postsPattern "content"
            renderAtom feedCfg feedCtx posts

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsPattern
            let archiveCtx =
                    listField "posts" (postCtx tags) (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/page.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    -- match "pages/index.html" $ do
    --
    paginateRules paginate $ \page pattern -> do
        route stripPages
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots pattern "content"
            let pagedCtx = paginateContextPlus paginate page
                indexCtx =
                    constField "title" (if page == 1 then "Latest blog posts" else "Blog posts, page " ++ show page) <>
                    listField "posts" (previewCtx tags) (return posts) <>
                    pagedCtx <>
                    mainCtx tags postsPattern

            makeItem ""
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/posts-preview-list.html" indexCtx
                >>= loadAndApplyTemplate "templates/page-right-column.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
stripPages = gsubRoute "pages/" $ const ""

mainCtx :: Tags -> Pattern -> Context String
mainCtx tags postsPattern =
    let recentPosts = postItems postsPattern >>= fmap (take 5) . recentFirst in
      listField "recentPosts" (previewCtx tags) recentPosts <>
      tagCloudField "tagCloud" 75 200 tags <>
      defaultContext

postCtx :: Tags -> Context String
postCtx tags =
    dateField "date" "%B %e, %Y" <>
    tagsField "tags" tags <>
    defaultContext

previewCtx :: Tags -> Context String
previewCtx tags = teaserField "preview" "content" <> postCtx tags

feedCfg :: FeedConfiguration
feedCfg = FeedConfiguration
    { feedTitle = "Mike Limansky blog"
    , feedDescription = "Latest blog posts"
    , feedAuthorName = "Mike Limansky"
    , feedAuthorEmail = "mike.limansky@gmail.com"
    , feedRoot = "http://www.limansky.me"
    }

-- Check argrumens for '--with-drafts'
-- returns post pattern, configuration, command arguments
checkArgs :: [String] -> (Pattern, Configuration, [String])
checkArgs args = case partition (/= "--with-drafts") args of
    (_, []) -> ("posts/*",                  defaultConfiguration,   args)
    (as, _) -> ("posts/*" .||. "drafts/*",  draftConf,              as)
    where draftConf = defaultConfiguration {
        destinationDirectory = "_draftSite"
      , storeDirectory = "_draftCache"
      , tmpDirectory = "_draftCache/tmp"
      }

postItems :: Pattern ->  Compiler [Item String]
postItems postsPattern = do
    identifiers <- getMatches postsPattern
    return [Item identifier "" | identifier <- identifiers]

postsGrouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
postsGrouper ids = liftM (paginateEvery 5) . sortRecentFirst $ ids

makePageId :: PageNumber -> Identifier
makePageId n = fromFilePath $ if (n == 1) then "index.html" else show n ++ "/index.html"

paginateContextPlus :: Paginate -> PageNumber -> Context a
paginateContextPlus pag currentPage = paginateContext pag currentPage <> mconcat
    [ listField "postsBefore" linkCtx pagesBefore
    , listField "postsAfter"  linkCtx pagesAfter
    ]
    where
        linkCtx = field "pageNum" (return . fst . itemBody) <> field "pageUrl" (return . snd . itemBody)
        lastPage = M.size . paginateMap $ pag
        pageInfo n = let i = paginateMakeId pag n in makeItem (show n, i)
        pages = [pageInfo n | n <- [1..lastPage], n /= currentPage]
        -- pagesBefore = sequence [ makeItem ("bbb", "ccc")]
        (pagesBefore, pagesAfter) = let (b, a) = span ((< currentPage) . fst) pages in (sequence b, sequence a)
