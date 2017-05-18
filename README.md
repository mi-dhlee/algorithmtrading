# algorithmtrading

eBest open api를 활용한 알고리즘 트레이딩 연습

http://htmlpreview.github.io/?https://github.com/mi-dhlee/algorithmtrading/blob/master/output.nb.html


1. 테스트 환경 

-windows 7 

-R 3.1.3 

-statconnDcom(http://rcom.univie.ac.at/download.html) 

-eBest api(www.ebestsec.co.kr), 모의투자 서버 활용




2. 기타 

-"C:/Git/" 경로에 파일 다운로드 

-오픈 API를 활용하여 주가 및 통계데이터를 수집 

-수집된 데이터를 클린징 후 주가예측 및 자산배분 

-자산 배분된 값을 바탕으로 주문 실행 




3. GetPrice.R

해당 파일은 이베스트 오픈 API를 사용하여 주가데이터 수집하는 예제를 담고 있다.API에 대한 자세한 설명은 이베스트 홈페이지를 통해 알 수 있으며 이를 응용하여 R 환경에서 각 tr 사용을 해보도록 하자. 단, 32비트 R 버전에서만 작동 가능하므로 메모리 관리에 유의하도록 한다.



4. GetStat.R 

해당 파일은 kosis 통계정보(http://kosis.kr/openapi/index/index.jsp) api를 사용하여 각 통계데이터를 수집하는 예제를 보여준다. 주가예측에 유용한 많은 종류의 데이터를 무료로 다운로드 받을 수 있다.



5. Alz.R 

해당 파일은 수집된 주가 및 통계데이터를 로딩하여 클린징 후 간단한 기계학습 및 자산배분을 하는 예제를 보여준다. 기계학습은 SVM을 사용하였으며 예시를 목적으로 하므로 각 feature의 quality은 별도로 고려하지 않았다. 예측 이후 Resampled-efficiency를 활용하여 자산 배분을 하는 예제도 추가하였다.



6. Order.R 

해당 파일은 자산배분 된 결과를 바탕으로 eBest open api를 활용하여 계좌조회, 매수 주문전송을 하는 예제를 담고 있다. 실제 활용시에는 지정가 주문 시 실거래 주가를 별도의 tr을 가지고 원하는 분봉으로 변환하도록 하자. 또한 분할 매수 시 별도의 주문집행 알고리즘(VWAP, PIN...)을 만들어 실행하길 바란다.
