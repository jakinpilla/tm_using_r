주어진 news 폴더 데이터는 네이버 랭킹뉴스 내 정치 부분에서 사람들이 가장 많이 본 뉴스 1위인 것만 크롤링한 데이터임.

labeled_dataset.csv 파일은 위의 news 데이터를 분류한 데이터로, Category 컬럼의 BH는 청와대, Con/Party는 국회/정당, North는 북한, Admin은 행정, Defence/Diplo는 국방/외교, Politic은 정치일반을 각각 의미함

news 데이터를 각 카테로리고 분류하는 분류 인공신경망 모델을 텐서플로우를 이용하여 만드는 것이 우리 조의 과제임.
