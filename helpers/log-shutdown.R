
write(sprintf('%s: DONE, shutting down.', now()), sprintf("%slog_FER-all.txt", "./logfiles/"), append = TRUE)
installr::os.shutdown(m = 15)
