Feature: Composer EDE project creation

  Scenario: Visit a file in the EDE project
    Given I visit "src/main.php" in project "with-composer"
    Then ede-php-autoload-project should exist


  Scenario: Load a basic PSR-0 class
    Given I visit "src/main.php" in project "with-composer"
    Then the class "Psr0Ns_TheClass" should be detected in "src/Psr0Ns/TheClass.php"

  Scenario: Load a PSR-0 class using fallback
    Given I visit "src/main.php" in project "with-composer"
    Then the class "Psr0Fallback_MyClass" should be detected in "src/Fallback/Psr0/Psr0Fallback/MyClass.php"


  Scenario: Load a basic PSR-4 class
    Given I visit "src/main.php" in project "with-composer"
    Then the class "Psr4Ns\TheClass" should be detected in "src/Psr4Ns/TheClass.php"

  Scenario: Load a PSR-4 multi-directory namespace
    Given I visit "src/main.php" in project "with-composer"
    Then the class "MultiDirNs\\TheClass1" should be detected in "src/MultiDirNs1/TheClass1.php"
    And the class "MultiDirNs\\TheClass2" should be detected in "src/MultiDirNs2/TheClass2.php"

  Scenario: Load a PSR-4 class using fallback
    Given I visit "src/main.php" in project "with-composer"
    Then the class "Psr4Fallback\MyClass" should be detected in "src/Fallback/Psr4/Psr4Fallback/MyClass.php"

  Scenario: Load a class the doesn't exist
    Given I visit "src/main.php" in project "with-composer"
    Then the class "Psr4Ns\DoesNotExist" should not be detected

  Scenario: Load an autoload-dev class
    Given I visit "src/main.php" in project "with-composer"
    Then the class "AutoloadDev\TestClass" should be detected in "src/AutoloadDev/TestClass.php"

  Scenario: Load a dependency class
    Given I visit "src/main.php" in project "with-composer"
    Then the class "ThirdParty\ThirdClass" should be detected in "vendor/third-party/third-party/src/ThirdClass.php"

  Scenario: Load a dependency with a target dir
    Given I visit "src/main.php" in project "with-composer"
    Then the class "TargetDir\Component\TheClass" should be detected in "vendor/target-dir/target-dir/TargetDir/Component/TheClass.php"

  Scenario: Load a composer dev dependency class
    Given I visit "src/main.php" in project "with-composer"
    Then the class "DevDependency\TestClass" should be detected in "vendor/third-party/dev-dependency/src/TestClass.php"
