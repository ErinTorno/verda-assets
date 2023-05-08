# Revision history for verda-assets

## 0.2.0.0 -- 2023-05-07

* Total rewrite and split into own git repository
* Supports asynchronously loading assets, with dependencies, sub assets, asset aliases, and metadata
* Weak vs. strong asset handles to allow automatic cleanup
* Runs in a MonadIO/MonadUnliftIO context to access Reader-like environments, such as ReaderT or RIO
