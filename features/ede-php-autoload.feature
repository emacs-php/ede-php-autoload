Feature: Hand-made EDE project creation

  Scenario: Visit a file in the ede project
    Given I visit "src/main.php" in project "without-composer"
    Then ede-php-autoload-project should exist
    And ede-php-autoload-project should have "." as include path
    And ede-php-autoload-project should have "/usr/share/php" as system include path

  Scenario: Visit a directory in the ede project
    Given I visit "." in project "without-composer"
    Then ede-php-autoload-project should exist

  Scenario: Load a basic PSR-0 class
    Given I visit "src/main.php" in project "without-composer"
    Then the class "Psr0Ns_TheClass" should be detected in "src/Psr0Ns/TheClass.php"

  Scenario: Load a split PSR-0 namespace
    Given I visit "src/main.php" in project "without-composer"
    Then the class "Psr0Split\Ns2\TheClass" should be detected in "src/Psr0Split/Ns2/TheClass.php"

  Scenario: Load a basic PSR-4 class
    Given I visit "src/main.php" in project "without-composer"
    Then the class "Psr4Ns\TheClass" should be detected in "src/Psr4Ns/TheClass.php"

  Scenario: Load a split PSR-4 namespace
    Given I visit "src/main.php" in project "without-composer"
    Then the class "Psr4Split\Ns2\TheClass" should be detected in "src/Psr4Split/Ns2/TheClass.php"

  Scenario: Load a PSR-4 multi-directory namespace
    Given I visit "src/main.php" in project "without-composer"
    Then the class "MultiDirNs\\TheClass1" should be detected in "src/MultiDirNs1/TheClass1.php"
    And the class "MultiDirNs\\TheClass2" should be detected in "src/MultiDirNs2/TheClass2.php"

  Scenario: Guess a PSR-4 class name
    Given I visit "src/main.php" in project "without-composer"
    Then guessing the class name for "src/MultiDirNs1/SubNs1/SubNs2/Class.php" should return "MultiDirNs\SubNs1\SubNs2\Class"

  Scenario: Guess a split PSR-4 class name
    Given I visit "src/main.php" in project "without-composer"
    Then guessing the class name for "src/Psr4Split/Ns2/MyClass.php" should return "Psr4Split\Ns2\MyClass"

  Scenario: Load a classmap class
    Given I visit "src/main.php" in project "without-composer"
    Then the class "ClassMapNs\MyClass" should be detected in "src/ClassMapNs/MyClass.php"

  Scenario: Guess a classmap class name
    Given I visit "src/main.php" in project "without-composer"
    THen guessing the class name for "src/ClassMapNs/MyClass.php" should return "ClassMapNs\MyClass"


  Scenario: Load a class that doesn't exist
    Given I visit "src/main.php" in project "without-composer"
    Then the class "Psr4Ns\DoesNotExist" should not be detected
