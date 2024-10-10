test_that("auth error checks", {
  test_token_file <- tempfile()
  expect_error(decrypt_gdrive_token(token_file = test_token_file))

  Sys.unsetenv("TEST_VAR")
  expect_error(decrypt_gdrive_token(token_file = test_token_file,
                                    decrypt_env_var = "TEST_VAR"))
})
