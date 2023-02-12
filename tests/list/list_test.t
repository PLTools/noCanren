  $ ./list_test_run.exe
  lenght, all answers {
  q=5;
  }
  fold_left, all answers {
  q=10;
  }
  fold_right, all answers {
  q=15;
  }
  assoc, all answers {
  q=10;
  }
  assoc - failure, all answers {
  }
  assoc backward, all answers {
  q=1;
  q=2;
  q=3;
  }
  fold_left backward, 10 answers {
  q=[10];
  q=[1; 10];
  q=[2; 5];
  q=[1; 1; 10];
  q=[1; 2; 5];
  q=[1; 1; 1; 10];
  q=[5; 2];
  q=[1; 1; 2; 5];
  q=[1; 1; 1; 1; 10];
  q=[1; 5; 2];
  }
