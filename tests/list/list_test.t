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
  fold_right2 backward, 10 answers {
  q=[10]; r=[1];
  q=[5]; r=[2];
  q=[2]; r=[5];
  q=[1]; r=[10];
  q=[10; 0]; r=[1; _.41];
  q=[5; 0]; r=[2; _.41];
  q=[2; 0]; r=[5; _.41];
  q=[1; 0]; r=[10; _.41];
  q=[10; 1]; r=[1; 0];
  q=[5; 1]; r=[2; 0];
  }
  mapi backward, 10 answers {
  q=[_.20; 0; 5];
  q=[_.20; 0; 2; 2];
  q=[_.20; 1; 0; 3];
  q=[_.20; 0; 0; 2; 1];
  q=[_.20; 0; 1; 0; 2];
  q=[_.20; 2; 4];
  q=[_.20; 0; 0; 0; 0; 2];
  q=[_.20; 1; 3; 1];
  q=[_.20; 2; 1; 2];
  q=[_.20; 0; 5; 0];
  }
